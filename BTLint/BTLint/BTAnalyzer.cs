using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Linq;
using System.Text;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;
using Microsoft.CodeAnalysis.Text;
using static BTAnalyzer.StringValidator;

namespace BTAnalyzer
{
    /// <summary>
    /// Analyses BT formatting issues.
    /// </summary>
    [DiagnosticAnalyzer(LanguageNames.CSharp)]
    public class BTAnalyzer : DiagnosticAnalyzer
    {
        /// <summary>
        /// Diagnostic ID.
        /// </summary>
        public const string DiagnosticId = "BTLint";

        /// <summary>
        /// Category.
        /// </summary>
        private const string Category = "CommentFormatting";

        /// <summary>
        /// CSharp.
        /// </summary>
        private const string CSharp = "C#";

        /// <summary>
        /// Class comment rule.
        /// </summary>
        private static DiagnosticDescriptor Rule = new DiagnosticDescriptor(BTAnalyzer.DiagnosticId, "Class comment error", "{0}", Category, DiagnosticSeverity.Warning, true);

        /// <summary>
        /// Supported Diagnostics.
        /// </summary>
        public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics { get { return ImmutableArray.Create(Rule); } }

        /// <summary>
        /// Initializes.
        /// </summary>
        /// <param name="context">Context.</param>
        public override void Initialize(AnalysisContext context)
        {
            // Analyze method, constructor declarations
            context.RegisterSyntaxNodeAction(AnalyzeMethodDeclaration, SyntaxKind.MethodDeclaration, SyntaxKind.ConstructorDeclaration, SyntaxKind.FieldDeclaration);

            // Analyze class, field, property, interface, enum, delegate, event declarations
            context.RegisterSyntaxNodeAction(AnalyzeClassDeclaration, SyntaxKind.ClassDeclaration, SyntaxKind.StructDeclaration, SyntaxKind.InterfaceDeclaration, SyntaxKind.EnumDeclaration, SyntaxKind.DelegateDeclaration,
                SyntaxKind.EventDeclaration, SyntaxKind.EventFieldDeclaration);

            // Analyze inside method items
            context.RegisterSyntaxNodeAction(AnalyzeBlockEmptyLines, SyntaxKind.ClassDeclaration);
            context.RegisterSyntaxNodeAction(AnalyzeInsideMethod, SyntaxKind.MethodDeclaration);
        }

        /// <summary>
        /// Analyzes method declaration.
        /// </summary>
        /// <param name="context">Syntax node analysis context.</param>
        private static void AnalyzeMethodDeclaration(SyntaxNodeAnalysisContext context)
        {
            if (BTAnalyzer.IsGenerated(context))
                return;
            BTAnalyzer.AnalyzeSummaryComments(context);
        }

        /// <summary>
        /// Analyzes class declaration.
        /// </summary>
        /// <param name="context">Syntax node analysis context.</param>
        private static void AnalyzeClassDeclaration(SyntaxNodeAnalysisContext context)
        {
            if (BTAnalyzer.IsGenerated(context))
                return;
            BTAnalyzer.AnalyzeSummaryComments(context);
        }

        /// <summary>
        /// Analyzes items inside methods.
        /// </summary>
        /// <param name="context">Code block analysis context.</param>
        private static void AnalyzeInsideMethod(SyntaxNodeAnalysisContext context)
        {
            if (BTAnalyzer.IsGenerated(context))
                return;

            // Analyze comments inside methods
            BTAnalyzer.AnalyzeInsideMethodComment(context);
            BTAnalyzer.AnalyzeLogicalExpressions(context);
        }

        /// <summary>
        /// Summary text validators.
        /// </summary>
        private static readonly StringValidator.Validate[] SummaryTextValidators = new StringValidator.Validate[]
        {
            StringValidator.CommentNotEmpty,
            StringValidator.EndWithDot,
            StringValidator.NoMultipleSpace,
            StringValidator.StartsWithSpace,
            StringValidator.StartWithCapitalLetter,
            StringValidator.FirstWordInSForm,
        };

        /// <summary>
        /// Summary text validators without start with s.
        /// </summary>
        private static readonly StringValidator.Validate[] SummaryTextValidatorsWithoutStartWithS = new StringValidator.Validate[]
        {
            StringValidator.CommentNotEmpty,
            StringValidator.EndWithDot,
            StringValidator.NoMultipleSpace,
            StringValidator.StartsWithSpace,
            StringValidator.StartWithCapitalLetter,
        };

        /// <summary>
        /// Parameter text validators.
        /// </summary>
        private static readonly StringValidator.Validate[] ParamTextValidators = new StringValidator.Validate[]
        {
            StringValidator.CommentNotEmpty,
            StringValidator.EndWithDot,
            StringValidator.NoMultipleSpace,
            StringValidator.StartWithCapitalLetter,
        };

        /// <summary>
        /// Return text validators.
        /// </summary>
        private static readonly StringValidator.Validate[] ReturnTextValidators = new StringValidator.Validate[]
        {
            StringValidator.CommentNotEmpty,
            StringValidator.EndWithDot,
            StringValidator.NoMultipleSpace,
            StringValidator.StartWithCapitalLetter,
        };

        /// <summary>
        /// Normal comment validator.
        /// </summary>
        private static readonly StringValidator.Validate[] NormalCommentValidator = new StringValidator.Validate[]
        {
            StringValidator.CommentNotEmpty,
            StringValidator.NotEndWithDot,
            StringValidator.StartWithCapitalLetter,
            StringValidator.NoMultipleSpace,
            StringValidator.FirstWordNotInSForm
        };

        /// <summary>
        /// Analyzes equal comments.
        /// </summary>
        /// <param name="context">Context.</param>
        private static void AnalyzeLogicalExpressions(SyntaxNodeAnalysisContext context)
        {
            // Get method declaration node
            MethodDeclarationSyntax methodDeclaration = context.Node as MethodDeclarationSyntax;
            if (null == methodDeclaration)
                return;

            // Get all equal family expressions
            // Check that constants are on the left
            IEnumerable<SyntaxNode> equalFamilyExpressions = methodDeclaration.DescendantNodes().Where(node => BTAnalyzer.IsEqualFamilyExpression(node.Kind()) && (SyntaxKind.ForStatement != node.Parent.Kind()));
            foreach (SyntaxNode equalNode in equalFamilyExpressions)
            {
                SyntaxNode[] twoSideExpressionNodes = equalNode.ChildNodes().ToArray();
                if (2 != twoSideExpressionNodes.Count())
                {
                    context.ReportDiagnostic(Diagnostic.Create(BTAnalyzer.Rule, equalNode.GetLocation(), ErrorCode.InvalidExpression));
                    continue;
                }
                if (!BTAnalyzer.IsConstant(twoSideExpressionNodes[0]) && BTAnalyzer.IsConstant(twoSideExpressionNodes[1]))
                    context.ReportDiagnostic(Diagnostic.Create(BTAnalyzer.Rule, equalNode.GetLocation(), ErrorCode.ConstantOnLeft));
            }

            // Get all logical expressions
            // Check that full parenthization is used in logical expressions
            // IEnumerable<SyntaxNode> logicalExpressions = methodDeclaration.DescendantNodes().Where(node => BTAnalyzer.IsLogicalExpression(node.Kind()));
            // foreach (SyntaxNode logicalNode in logicalExpressions)
            // {
            //    foreach (SyntaxNode node in logicalNode.ChildNodes().Where(node => BTAnalyzer.IsEqualFamilyExpression(node.Kind())))
            //        context.ReportDiagnostic(Diagnostic.Create(BTAnalyzer.Rule, node.GetLocation(), ErrorCode.MissingFullParenthization));
            // }

            // Check that statements with 1 expression statement should ignore parenthesis
            IEnumerable<SyntaxNode> blockStatements = methodDeclaration.DescendantNodes().Where(node => BTAnalyzer.IsBlockStatement(node.Kind()));
            foreach (SyntaxNode node in blockStatements)
            {
                BlockSyntax block = node.ChildNodes().Where(nodee => SyntaxKind.Block == nodee.Kind()).OfType<BlockSyntax>().FirstOrDefault();
                if (null == block)
                    continue;
                IEnumerable<SyntaxNode> expressionNodes = block.ChildNodes().Where(nodee => BTAnalyzer.IsSingleLineStatement(nodee));
                if ((1 == expressionNodes.Count()) && (1 == block.ChildNodes().Count()))
                    context.ReportDiagnostic(Diagnostic.Create(BTAnalyzer.Rule, node.GetLocation(), ErrorCode.UnnecessaryBlock));
            }
        }

        /// <summary>
        /// Analyzes comment inside block.
        /// </summary>
        /// <param name="context">Context.</param>
        private static void AnalyzeInsideMethodComment(SyntaxNodeAnalysisContext context)
        {
            // Get the block
            MethodDeclarationSyntax methodDeclarationSyntax = context.Node as MethodDeclarationSyntax;
            if (null == methodDeclarationSyntax)
                return;

            // Get all single line comment trivia
            IEnumerable<SyntaxTrivia> singleLineCommentTrivias = methodDeclarationSyntax.Body.DescendantTrivia().Where(trivia => (SyntaxKind.SingleLineCommentTrivia == trivia.Kind()) ||
                (SyntaxKind.SingleLineDocumentationCommentTrivia == trivia.Kind()));

            // Iterate through each single line comment
            string message = string.Empty;
            foreach (SyntaxTrivia singleLineComment in singleLineCommentTrivias)
            {
                Location location = singleLineComment.GetLocation();
                Position position = Position.Origin;
                if (!StringValidator.StartWithTwoSlashes(singleLineComment.ToString(), ref message, ref position))
                {
                    context.ReportDiagnostic(Diagnostic.Create(BTAnalyzer.Rule, BTAnalyzer.GetLocation(singleLineComment.SyntaxTree, location, position), message));
                    continue;
                }
                string trimmedText = singleLineComment.ToString().Substring(3);
                foreach (StringValidator.Validate validate in BTAnalyzer.NormalCommentValidator)
                {
                    if (!validate(trimmedText, ref message, ref position))
                        context.ReportDiagnostic(Diagnostic.Create(BTAnalyzer.Rule, BTAnalyzer.GetLocation(singleLineComment.SyntaxTree, location, position, 3), message));
                }
            }
        }

        /// <summary>
        /// Analyzes empty lines inside blocks.
        /// </summary>
        /// <param name="context">Context.</param>
        private static void AnalyzeBlockEmptyLines(SyntaxNodeAnalysisContext context)
        {
            // Return early
            if (BTAnalyzer.IsGenerated(context))
                return;

            // Get the block
            ClassDeclarationSyntax classDeclarationSyntax = context.Node as ClassDeclarationSyntax;

            // Check method spacing
            SyntaxNode[] nodes = classDeclarationSyntax.ChildNodes().Where(node => SyntaxKind.BaseList != node.Kind() && SyntaxKind.AttributeList != node.Kind()).ToArray();
            Location location = default(Location);
            for (int i = 0; i < nodes.Length; i++)
            {
                SyntaxNode method = nodes[i];
                IEnumerable<SyntaxTrivia> trivias = method.GetFirstToken().LeadingTrivia;
                int count = 0;
                int j = 0;
                foreach (SyntaxTrivia trivia in trivias)
                {
                    if (j == 0)
                    {
                        location = BTAnalyzer.GetLocation(trivia.SyntaxTree, trivia.GetLocation(), Position.Origin);
                        j++;
                    }

                    if (SyntaxKind.EndOfLineTrivia == trivia.Kind())
                        count++;
                    else if (!(SyntaxKind.WhitespaceTrivia == trivia.Kind()))
                        break;
                }
                if ((i == 0 && count > 0) || (i > 0 && count > 1))
                    context.ReportDiagnostic(Diagnostic.Create(BTAnalyzer.Rule, location, ErrorCode.ExtraLine));
                if (i > 0 && count < 1)
                    context.ReportDiagnostic(Diagnostic.Create(BTAnalyzer.Rule, location, ErrorCode.MissingEmptyLine));
            }


            // Check blocks
            IEnumerable<BlockSyntax> blocks = classDeclarationSyntax.DescendantNodes().Where(node => SyntaxKind.Block == node.Kind()).OfType<BlockSyntax>();

            // Iterate through each block
            foreach (BlockSyntax block in blocks)
            {
                // Get a list of node, single line comment and end of line
                List<Tuple<object, SyntaxKind>> blockObjectList = new List<Tuple<object, SyntaxKind>>();
                foreach (SyntaxNode node in block.ChildNodes())
                {
                    // Check whether the node is a block node
                    bool isBlockNode = node.ChildNodes().Any(nodee => SyntaxKind.Block == nodee.Kind());

                    // Get all trivias
                    SyntaxTrivia[] allTrivias = node.DescendantTrivia().Where(trivia => (SyntaxKind.SingleLineCommentTrivia == trivia.Kind()) ||
                        (SyntaxKind.EndOfLineTrivia == trivia.Kind())).ToArray();

                    // Add trivias before the node
                    foreach (SyntaxTrivia trivia in allTrivias)
                    {
                        if (trivia.Span.End <= node.SpanStart)
                            blockObjectList.Add(Tuple.Create<object, SyntaxKind>(trivia, trivia.Kind()));
                    }

                    // Add the node
                    blockObjectList.Add(Tuple.Create<object, SyntaxKind>(node, SyntaxKind.None));

                    // Add trivia after the node
                    foreach (SyntaxTrivia trivia in allTrivias)
                    {
                        if ((trivia.SpanStart >= node.Span.End) && !isBlockNode)
                            blockObjectList.Add(Tuple.Create<object, SyntaxKind>(trivia, trivia.Kind()));
                    }

                    // Add an additional end of line trivia for block node
                    if (isBlockNode)
                        blockObjectList.Add(Tuple.Create<object, SyntaxKind>(null, SyntaxKind.EndOfLineTrivia));
                }

                // Trim the block object list
                // Remove end of line after a statement
                List<Tuple<object, SyntaxKind>> trimmedBlockObjectList = new List<Tuple<object, SyntaxKind>>();
                for (int i = 0; i < blockObjectList.Count(); i++)
                {
                    if (SyntaxKind.EndOfLineTrivia != blockObjectList[i].Item2)
                    {
                        if ((i + 1 < blockObjectList.Count()) && (SyntaxKind.EndOfLineTrivia == blockObjectList[i + 1].Item2))
                        {
                            trimmedBlockObjectList.Add(blockObjectList[i]);
                            i++;
                            continue;
                        }
                    }
                    trimmedBlockObjectList.Add(blockObjectList[i]);
                }

                // Cannot start with empty lines
                for (int i = 0; i < trimmedBlockObjectList.Count(); i++)
                {
                    if (SyntaxKind.EndOfLineTrivia == blockObjectList[i].Item2)
                    {
                        SyntaxTrivia trivia = (SyntaxTrivia)trimmedBlockObjectList[i].Item1;
                        context.ReportDiagnostic(Diagnostic.Create(BTAnalyzer.Rule, trivia.GetLocation(), ErrorCode.ExtraLine));
                    }
                    else
                        break;
                }

                // Check if there is only syntax node inside the block
                bool isOnlySyntaxNode = trimmedBlockObjectList.All(tuple => SyntaxKind.None == tuple.Item2);

                // Statement should have comments before
                for (int i = 0; i < trimmedBlockObjectList.Count(); i++)
                {
                    // Skip nodes
                    while ((i < trimmedBlockObjectList.Count()) && (SyntaxKind.None != trimmedBlockObjectList[i].Item2))
                        i++;

                    // Check for missing comment
                    if (i < trimmedBlockObjectList.Count())
                    {
                        if ((((0 < i - 1) && (SyntaxKind.SingleLineCommentTrivia != trimmedBlockObjectList[i - 1].Item2)) || i == 0) && !isOnlySyntaxNode)
                        {
                            SyntaxNode node = (SyntaxNode)trimmedBlockObjectList[i].Item1;
                            context.ReportDiagnostic(Diagnostic.Create(BTAnalyzer.Rule, Location.Create(node.SyntaxTree, new TextSpan(node.SpanStart, 10)), ErrorCode.MissingComment));
                        }
                    }
                    while ((i < trimmedBlockObjectList.Count()) && (SyntaxKind.None == trimmedBlockObjectList[i].Item2))
                        i++;

                    // Check for missing empty line
                    if ((i < trimmedBlockObjectList.Count()) && (SyntaxKind.EndOfLineTrivia != trimmedBlockObjectList[i].Item2))
                        context.ReportDiagnostic(Diagnostic.Create(BTAnalyzer.Rule, ((SyntaxTrivia)trimmedBlockObjectList[i].Item1).GetLocation(), ErrorCode.MissingEmptyLine));
                }

                // Cannot end with empty lines
                IEnumerable<SyntaxTrivia> triviaBeforeClosingBrackets = block.CloseBraceToken.LeadingTrivia;
                if (1 < triviaBeforeClosingBrackets.Count())
                    context.ReportDiagnostic(Diagnostic.Create(BTAnalyzer.Rule, block.CloseBraceToken.GetLocation(), ErrorCode.UnexpectedComponentsBeforeClosingBracket));
            }
        }

        /// <summary>
        /// Analyzes class comments.
        /// </summary>
        /// <param name="context">Context.</param>
        private static void AnalyzeSummaryComments(SyntaxNodeAnalysisContext context)
        {
            // Get declaration node
            SyntaxNode declarationNode = context.Node;
            if (null == declarationNode)
                return;

            // Check all trivia under the first token
            SyntaxToken? firstToken = declarationNode?.DescendantTokens().FirstOrDefault();

            // No single line comment
            IEnumerable<SyntaxTrivia> singleLineCommentTrivias = firstToken?.GetAllTrivia().Where(trivia => SyntaxKind.SingleLineCommentTrivia == trivia.Kind());
            if (0 < singleLineCommentTrivias.Count())
            {
                BTAnalyzer.ReportDiagnostic(context, singleLineCommentTrivias.First().GetLocation(), ErrorCode.ClassCommentStart);
                return;
            }

            // Only 1 single documentation line comment
            IEnumerable<SyntaxTrivia> singleDocumentationCommentTrivias = firstToken?.GetAllTrivia().Where(trivia => SyntaxKind.SingleLineDocumentationCommentTrivia == trivia.Kind());
            if (0 == singleDocumentationCommentTrivias.Count())
            {
                BTAnalyzer.ReportDiagnostic(context, declarationNode.GetFirstToken().GetLocation(), ErrorCode.MissingComment);
                return;
            }
            else if (1 < singleDocumentationCommentTrivias.Count())
            {
                BTAnalyzer.ReportDiagnostic(context, declarationNode.GetFirstToken().GetLocation(), ErrorCode.ErrorsInComment);
                return;
            }

            // Get the only single line documentation comment trivia
            SyntaxTrivia singleDocCommentTrivia = singleDocumentationCommentTrivias.First();
            List<string> messageList = new List<string>();
            List<Location> locationList = new List<Location>();
            List<string> paramCommentNameList = new List<string>();
            int returnCount = 0;
            if (!BTAnalyzer.CheckDocumentationCommentTrivia(singleDocCommentTrivia, declarationNode.Kind(), ref messageList, ref locationList, ref paramCommentNameList, ref returnCount))
            {
                for (int i = 0; i < messageList.Count(); i++)
                    BTAnalyzer.ReportDiagnostic(context, locationList[i], messageList[i]);
            }

            // Check method declaration
            // Check parameter comments
            MethodDeclarationSyntax methodDeclarationSyntax = declarationNode as MethodDeclarationSyntax;
            if (null == methodDeclarationSyntax)
                return;

            // Check parameter comment count
            IEnumerable<ParameterSyntax> parameters = methodDeclarationSyntax.ParameterList.Parameters;
            if (paramCommentNameList.Count() != parameters.Count())
                BTAnalyzer.ReportDiagnostic(context, methodDeclarationSyntax.ParameterList.GetLocation(), ErrorCode.IncompatibleParamComments);

            // Check that all parameters have comment
            foreach (ParameterSyntax parameter in parameters)
            {
                if (!paramCommentNameList.Any(p => p == parameter.Identifier.Text))
                {
                    BTAnalyzer.ReportDiagnostic(context, parameter.GetLocation(), ErrorCode.MissingParamComment);
                    break;
                }
            }

            // Check number of returns comments
            if ("void" == methodDeclarationSyntax.ReturnType.ToString())
            {
                if (0 < returnCount)
                {
                    BTAnalyzer.ReportDiagnostic(context, methodDeclarationSyntax.ReturnType.GetLocation(), ErrorCode.IncorrectReturnComment);
                    return;
                }
            }
            else
            {
                if (1 != returnCount)
                {
                    BTAnalyzer.ReportDiagnostic(context, methodDeclarationSyntax.ReturnType.GetLocation(), ErrorCode.IncorrectReturnComment);
                    return;
                }
            }
        }

        /// <summary>
        /// Checks documentation comment trivia.
        /// </summary>
        /// <param name="commentTria">Comment trivia.</param>
        /// <param name="nodeKind">Kind of node.</param>
        /// <param name="messageList">Message list.</param>
        /// <param name="locationList">Location list.</param>
        /// <param name="paramCommentNameList">Parameter comment name list.</param>
        /// <param name="returnCount">Return count.</param>
        /// <returns>Ture if there is no error.</returns>
        private static bool CheckDocumentationCommentTrivia(SyntaxTrivia commentTria, SyntaxKind nodeKind, ref List<string> messageList, ref List<Location> locationList, ref List<string> paramCommentNameList, ref int returnCount)
        {
            // Preset result to true
            bool result = true;

            // Return early if the trivia has no structure
            if (!commentTria.HasStructure)
            {
                messageList.Add(ErrorCode.ClassCommentStart);
                return false;
            }

            // Check XML elements
            SyntaxNode commentNode = commentTria.GetStructure();
            IEnumerable<XmlElementSyntax> xmlElements = commentNode.DescendantNodes().Where(node => typeof(XmlElementSyntax) == node.GetType()).OfType<XmlElementSyntax>();

            // Return false if no XML element is found 
            if (0 == xmlElements.Count())
            {
                messageList.Add(ErrorCode.ErrorsInComment);
                return false;
            }

            // Check each XML element
            Location location = commentTria.GetLocation();
            string message = string.Empty;
            foreach (XmlElementSyntax xmlElement in xmlElements)
            {
                // Swtich
                switch (xmlElement.StartTag.Name.ToString())
                {
                    case "summary":
                    {
                        if (!BTAnalyzer.CheckSummaryElement(nodeKind, xmlElement, ref location, ref message))
                        {
                            result = false;
                            messageList.Add(message);
                            locationList.Add(location);
                        }
                        break;
                    }

                    case "param":
                    {
                        if (!BTAnalyzer.CheckParamElement(xmlElement, ref location, ref message, ref paramCommentNameList))
                        {
                            result = false;
                            messageList.Add(message);
                            locationList.Add(location);
                        }
                        break;
                    }

                    case "returns":
                    {
                        if (!BTAnalyzer.CheckReturnElement(xmlElement, ref location, ref message, ref returnCount))
                        {
                            result = false;
                            messageList.Add(message);
                            locationList.Add(location);
                        }
                        break;
                    }
                }
            }

            // Return true
            return result;
        }

        /// <summary>
        /// Checks returns element.
        /// </summary>
        /// <param name="xmlElement">XML element.</param>
        /// <param name="location">Location.</param>
        /// <param name="message">Message.</param>
        /// <param name="returnCount">Return count.</param>
        /// <returns>True if valid.</returns>
        private static bool CheckReturnElement(XmlElementSyntax xmlElement, ref Location location, ref string message, ref int returnCount)
        {
            // Check tags
            if (!BTAnalyzer.CheckXmlTags(xmlElement, ref location, ref message))
                return false;

            // Increment number of <returns>
            returnCount++;

            // Set location
            location = xmlElement.StartTag.GetLocation();
            Position position = Position.Origin;

            // Check 
            if (xmlElement.ToString().Contains("\r\n"))
            {
                string[] lines = xmlElement.ToString().Split(new string[] { "///" }, StringSplitOptions.None);
                int offset = lines[0].Length + 3;
                for (int i = 1; i < lines.Length; i++)
                {
                    string trimmedLine = lines[i].TrimEnd(' ', '\r', '\n');
                    if (' ' != trimmedLine[0])
                    {
                        message = ErrorCode.MissingSpace;
                        location = BTAnalyzer.GetLocation(xmlElement.SyntaxTree, location, new Position(0, 0), offset);
                        return false;
                    }

                    // Ignore first and last line
                    if ((0 < i) && (i < lines.Length - 1))
                    {
                        // Validate text
                        foreach (StringValidator.Validate validate in BTAnalyzer.ReturnTextValidators)
                        {
                            if (!validate(trimmedLine, ref message, ref position))
                            {
                                location = BTAnalyzer.GetLocation(xmlElement.SyntaxTree, location, position, offset);
                                return false;
                            }
                        }
                    }

                    // Add offset, 3 for the removed ///
                    offset += lines[i].Length + 3;
                }
            }
            else
            {
                string text = xmlElement.ToString().Replace(xmlElement.StartTag.ToString(), string.Empty).Replace(xmlElement.EndTag.ToString(), string.Empty);
                string trimmedLine = text.TrimEnd(' ', '\r', '\n');
                foreach (StringValidator.Validate validate in BTAnalyzer.ReturnTextValidators)
                {
                    if (!validate(trimmedLine, ref message, ref position))
                    {
                        location = BTAnalyzer.GetLocation(xmlElement.SyntaxTree, location, position, xmlElement.StartTag.ToString().Length);
                        return false;
                    }
                }
            }

            // Return true
            return true;
        }

        /// <summary>
        /// Checks param element.
        /// </summary>
        /// <param name="xmlElement">XML element.</param>
        /// <param name="location">Location.</param>
        /// <param name="message">Message.</param>
        /// <param name="paramCommentNameList">Parameter comment name list.</param>
        /// <returns>True if there is no error.</returns>
        private static bool CheckParamElement(XmlElementSyntax xmlElement, ref Location location, ref string message, ref List<string> paramCommentNameList)
        {
            // Check tags
            if (!BTAnalyzer.CheckXmlTags(xmlElement, ref location, ref message))
                return false;

            // Set location
            location = xmlElement.StartTag.GetLocation();
            Position position = Position.Origin;

            // Add param name to the list
            XmlNameAttributeSyntax xmlNameAttribute = xmlElement.StartTag.Attributes.Where(attr => SyntaxKind.XmlNameAttribute == attr.Kind()).FirstOrDefault() as XmlNameAttributeSyntax;
            if (null == xmlNameAttribute)
            {
                message = ErrorCode.MissingNameAttribute;
                return false;
            }
            paramCommentNameList.Add(xmlNameAttribute.Identifier.ToString());

            // Remove <see cref .. /> elements, remove start and end tags
            // Check XML element text
            string text = xmlElement.ToString().Replace(xmlElement.StartTag.ToString(), String.Empty).Replace(xmlElement.EndTag.ToString(), String.Empty).TrimStart('/');
            foreach (StringValidator.Validate validate in BTAnalyzer.ParamTextValidators)
            {
                if (!validate(text, ref message, ref position))
                {
                    location = BTAnalyzer.GetLocation(xmlElement.SyntaxTree, location, position, xmlElement.StartTag.ToString().Length);
                    return false;
                }
            }

            // Return true
            return true;
        }

        /// <summary>
        /// Checks summary element.
        /// </summary>
        /// <param name="nodeKind">Kind of node.</param>
        /// <param name="xmlElement">The XML element.</param>
        /// <param name="location">Location.</param>
        /// <param name="message">Message.</param>
        /// <returns>True if there is no error.</returns>
        private static bool CheckSummaryElement(SyntaxKind nodeKind, XmlElementSyntax xmlElement, ref Location location, ref string message)
        {
            // Check xml start/end tags
            if (!BTAnalyzer.CheckXmlTags(xmlElement, ref location, ref message))
                return false;

            // Set location
            location = xmlElement.StartTag.GetLocation();
            Position position = Position.Origin;

            // Check XML element text
            string[] lines = xmlElement.ToString().Split(new string[] { "///" }, StringSplitOptions.None);

            // Check 
            int offset = lines[0].Length + 3;
            for (int i = 1; i < lines.Length; i++)
            {
                string trimmedLine = lines[i].TrimEnd(' ', '\r', '\n');
                if (' ' != trimmedLine[0])
                {
                    message = ErrorCode.MissingSpace;
                    location = BTAnalyzer.GetLocation(xmlElement.SyntaxTree, location, new Position(0, 0), offset);
                    return false;
                }

                // Ignore first and last lines
                if ((0 < i) && (i < lines.Length - 1))
                {
                    // Validate text
                    foreach (StringValidator.Validate validate in BTAnalyzer.GetSummaryValidator(nodeKind))
                    {
                        if (!validate(trimmedLine, ref message, ref position))
                        {
                            location = BTAnalyzer.GetLocation(xmlElement.SyntaxTree, location, position, offset);
                            return false;
                        }
                    }
                }

                // Increase offset
                offset += lines[i].Length + 3;
            }

            // Return true
            return true;
        }

        /// <summary>
        /// Gets summary validator.
        /// </summary>
        /// <param name="nodeKind">Kind of node.</param>
        /// <returns>Validator.</returns>
        private static IEnumerable<StringValidator.Validate> GetSummaryValidator(SyntaxKind nodeKind)
        {
            switch (nodeKind)
            {
                case SyntaxKind.MethodDeclaration:
                case SyntaxKind.ConstructorDeclaration:
                case SyntaxKind.ClassDeclaration:
                case SyntaxKind.StructDeclaration:
                case SyntaxKind.InterfaceDeclaration:
                return BTAnalyzer.SummaryTextValidators;
                case SyntaxKind.EnumDeclaration:
                case SyntaxKind.DelegateDeclaration:
                case SyntaxKind.EventDeclaration:
                case SyntaxKind.EventFieldDeclaration:
                case SyntaxKind.FieldDeclaration:
                return BTAnalyzer.SummaryTextValidatorsWithoutStartWithS;
                default:
                return null;
            }
        }

        /// <summary>
        /// Checks XML start and end tags.
        /// </summary>
        /// <param name="xmlElement">XML element.</param>
        /// <param name="location">Location.</param>
        /// <param name="message">Message.</param>
        /// <returns>True if valid.</returns>
        private static bool CheckXmlTags(XmlElementSyntax xmlElement, ref Location location, ref string message)
        {
            // Start/End tag
            string startTag = xmlElement.StartTag.Name.ToString();
            string endTag = xmlElement.EndTag.Name.ToString();
            if (string.IsNullOrWhiteSpace(startTag) || string.IsNullOrWhiteSpace(endTag))
            {
                message = ErrorCode.ErrorsInComment;
                location = xmlElement.GetLocation();
                return false;
            }

            // Only allows summary or param tag
            if (!BTAnalyzer.IsValidXmlCommentTag(startTag) || !BTAnalyzer.IsValidXmlCommentTag(endTag))
            {
                message = ErrorCode.InvalidXmlTag;
                location = xmlElement.GetLocation();
                return false;
            }

            // Return true
            return true;
        }

        /// <summary>
        /// Reports diagnostic.
        /// </summary>
        /// <param name="context">Context.</param>
        /// <param name="location">Location.</param>
        /// <param name="message">Message.</param>
        private static void ReportDiagnostic(SyntaxNodeAnalysisContext context, Location location, string message)
        {
            var diagnostic = Diagnostic.Create(Rule, location, message);
            context.ReportDiagnostic(diagnostic);
        }

        /// <summary>
        /// Checks whether xml comment tag is valid.
        /// </summary>
        /// <param name="tagName">Name of tag.</param>
        /// <returns>True if valid.</returns>
        private static bool IsValidXmlCommentTag(string tagName)
        {
            return ("summary" == tagName) || ("param" == tagName) || ("returns" == tagName);
        }

        /// <summary>
        /// Checks whether the expression belongs to the equal expression family.
        /// </summary>
        /// <param name="kind">Kind of node.</param>
        /// <returns>True if equal expression.</returns>
        internal static bool IsEqualFamilyExpression(SyntaxKind kind)
        {
            return (SyntaxKind.EqualsExpression == kind)
                || (SyntaxKind.NotEqualsExpression == kind)
                || (SyntaxKind.LessThanExpression == kind)
                || (SyntaxKind.LessThanOrEqualExpression == kind)
                || (SyntaxKind.GreaterThanExpression == kind)
                || (SyntaxKind.GreaterThanOrEqualExpression == kind);
        }

        /// <summary>
        /// Checks whether the expression is a logical expression.
        /// </summary>
        /// <param name="kind">Kind of node.</param>
        /// <returns>True if logical expression.</returns>
        private static bool IsLogicalExpression(SyntaxKind kind)
        {
            return (SyntaxKind.LogicalAndExpression == kind) || (SyntaxKind.LogicalNotExpression == kind) || (SyntaxKind.LogicalOrExpression == kind);
        }

        /// <summary>
        /// Checks whether the expression is a block statements.
        /// </summary>
        /// <param name="kind">Kind of node.</param>
        /// <returns>True if block statements.</returns>
        private static bool IsBlockStatement(SyntaxKind kind)
        {
            return (SyntaxKind.DoStatement == kind)
                || (SyntaxKind.ForEachStatement == kind)
                || (SyntaxKind.ForStatement == kind)
                || (SyntaxKind.IfStatement == kind)
                || (SyntaxKind.WhileStatement == kind)
                || (SyntaxKind.UsingStatement == kind);
        }

        /// <summary>
        /// Checks whether the statement is a single line statement.
        /// Single line statement does not have comment.
        /// </summary>
        /// <param name="node">Node.</param>
        /// <returns>True if single line.</returns>
        private static bool IsSingleLineStatement(SyntaxNode node)
        {
            string nodeString = node.ToFullString();
            return !nodeString.TrimEnd('\r', '\n').Contains(Environment.NewLine);
        }

        /// <summary>
        /// Checks whether a node represents a constant value.
        /// </summary>
        /// <param name="syntaxNode">Syntax node.</param>
        /// <returns>True if constant.</returns>
        private static bool IsConstant(SyntaxNode syntaxNode)
        {
            // Check whether the node itself has a constant value
            if ((SyntaxKind.NumericLiteralExpression == syntaxNode.Kind()) || (SyntaxKind.StringLiteralExpression == syntaxNode.Kind()) || char.IsUpper(syntaxNode.ToString()[0])
                || (SyntaxKind.CharacterLiteralExpression == syntaxNode.Kind()) || (SyntaxKind.NullLiteralExpression == syntaxNode.Kind()))
                return true;

            // Get the first node
            SyntaxNode firstNode = syntaxNode.ChildNodes().FirstOrDefault();
            if (null == firstNode)
                return false;

            // Return true if first node is a predefind type, e.g. int.MaxValue
            return SyntaxKind.PredefinedType == firstNode.Kind();
        }

        /// <summary>
        /// Gets exact location of an error.
        /// </summary>
        /// <param name="syntaxTree">Syntax tree.</param>
        /// <param name="location">Location.</param>
        /// <param name="position">Position.</param>
        /// <param name="offset">Offset.</param>
        /// <returns>Exact location.</returns>
        private static Location GetLocation(SyntaxTree syntaxTree, Location location, Position position, int offset = 0)
        {
            return Location.Create(syntaxTree, new TextSpan(location.SourceSpan.Start + position.Start + offset, position.Len));
        }

        /// <summary>
        /// Checks whether a file is a generated file.
        /// </summary>
        /// <param name="context">Context.</param>
        /// <returns>True if generated.</returns>
        private static bool IsGenerated(SyntaxNodeAnalysisContext context)
        {
            string path = Path.GetFileName(context.Node.SyntaxTree.FilePath);
            return path.Contains(".Generated") || path.Contains(".generated");
        }
    }
}

