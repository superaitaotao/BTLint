using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;

namespace Analyzer1
{
    /// <summary>
    /// An analyzer for comments.
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
        /// <param name="context"></param>
        public override void Initialize(AnalysisContext context)
        {
            context.RegisterSyntaxNodeAction(AnalyzeMethodDeclaration, SyntaxKind.MethodDeclaration);
            context.RegisterSyntaxNodeAction(AnalyzeClassDeclaration, SyntaxKind.ClassDeclaration);
            context.RegisterCodeBlockAction(AnalyzeInsideMethod);
        }

        private static void AnalyzeMethodDeclaration(SyntaxNodeAnalysisContext context)
        {
            BTAnalyzer.AnalyzeSummaryComments(context);
        }

        private static void AnalyzeClassDeclaration(SyntaxNodeAnalysisContext context)
        {
            BTAnalyzer.AnalyzeSummaryComments(context);
        }

        private static void AnalyzeInsideMethod(CodeBlockAnalysisContext context)
        {
            BTAnalyzer.AnalyzeInsideMethodComment(context);
            BTAnalyzer.AnalyzeBlockEmptyLines(context);
            BTAnalyzer.AnalyzeEqualStatements(context);
        }

        private static readonly StringValidator.Validate[] SummaryTextValidators = new StringValidator.Validate[]
        {
            StringValidator.CommentNotEmpty,
            StringValidator.EndWithDot,
            StringValidator.NoMultipleSpace,
            StringValidator.StartsWithSpace,
            StringValidator.StartWithCapitalLetter,
            StringValidator.FirstWordInSForm,
        };


        private static readonly StringValidator.Validate[] ParamTextValidators = new StringValidator.Validate[]
        {
            StringValidator.CommentNotEmpty,
            StringValidator.EndWithDot,
            StringValidator.NoMultipleSpace,
            StringValidator.NotStartsWithSpace,
            StringValidator.StartWithCapitalLetter,
        };

        private static readonly StringValidator.Validate[] ReturnTextValidators = new StringValidator.Validate[]
        {
            StringValidator.CommentNotEmpty,
            StringValidator.EndWithDot,
            StringValidator.NoMultipleSpace,
            StringValidator.NotStartsWithSpace,
            StringValidator.StartWithCapitalLetter,
        };

        private static readonly StringValidator.Validate[] NormalCommentValidator = new StringValidator.Validate[]
        {
            StringValidator.CommentNotEmpty,
            StringValidator.NotEndWithDot,
            StringValidator.StartWithCapitalLetter,
            StringValidator.NoMultipleSpace,
            StringValidator.FirstWordNotInSForm
        };

        private static void AnalyzeEqualStatements(CodeBlockAnalysisContext context)
        {
            MethodDeclarationSyntax methodDeclaration = context.CodeBlock as MethodDeclarationSyntax;
            IEnumerable<SyntaxNode> equalFamilyExpressions = methodDeclaration.DescendantNodes().Where(node => BTAnalyzer.IsEqualFamilyExpression(node.Kind()));
            foreach(SyntaxNode equalNode in equalFamilyExpressions)
            {
                SyntaxNode[] twoSideExpressionNodes = equalNode.ChildNodes().ToArray();
                if(twoSideExpressionNodes.Count() != 2)
                {
                    context.ReportDiagnostic(Diagnostic.Create(BTAnalyzer.Rule, equalNode.GetLocation(), ErrorCode.InvalidExpression));
                    continue;
                }

                if(!BTAnalyzer.IsConstant(twoSideExpressionNodes[0]) && BTAnalyzer.IsConstant(twoSideExpressionNodes[1]))
                    context.ReportDiagnostic(Diagnostic.Create(BTAnalyzer.Rule, equalNode.GetLocation(), ErrorCode.ConstantOnLeft));
            }
        }

        private static bool IsConstant(SyntaxNode syntaxNode)
        {
            if (syntaxNode.Kind() == SyntaxKind.NumericLiteralExpression || syntaxNode.Kind() == SyntaxKind.StringLiteralExpression || char.IsUpper(syntaxNode.ToString()[0]))
                return true;

            SyntaxNode firstNode = syntaxNode.ChildNodes().FirstOrDefault();
            if (null == firstNode)
                return false;

            return firstNode.Kind() == SyntaxKind.PredefinedType;
        }

        /// <summary>
        /// Analyze comment inside block.
        /// </summary>
        /// <param name="context">Context.</param>
        private static void AnalyzeInsideMethodComment(CodeBlockAnalysisContext context)
        {
            // Get the block
            MethodDeclarationSyntax methodDeclarationSyntax = context.CodeBlock as MethodDeclarationSyntax;
            if (methodDeclarationSyntax == null)
                return;

            // Get all single line comment trivia
            IEnumerable<SyntaxTrivia> singleLineCommentTrivias = methodDeclarationSyntax.GetTrailingTrivia().Where(trivia => SyntaxKind.SingleLineCommentTrivia == trivia.Kind() ||
                SyntaxKind.SingleLineDocumentationCommentTrivia == trivia.Kind());

            // Iterate through each single line comment
            string message = string.Empty;
            foreach (SyntaxTrivia singleLineComment in singleLineCommentTrivias)
            {
                if (!StringValidator.StartWithTwoSlashes(singleLineComment.ToString(), ref message))
                {
                    context.ReportDiagnostic(Diagnostic.Create(BTAnalyzer.Rule, singleLineComment.GetLocation(), message));
                    continue;
                }

                string trimmedText = singleLineComment.ToString().Substring(3);
                foreach (StringValidator.Validate validate in BTAnalyzer.NormalCommentValidator)
                {
                    if (!validate(trimmedText, ref message))
                        context.ReportDiagnostic(Diagnostic.Create(BTAnalyzer.Rule, singleLineComment.GetLocation(), message));
                }
            }
        }

        /// <summary>
        /// Analyzes empty lines inside blocks
        /// </summary>
        /// <param name="context"></param>
        private static void AnalyzeBlockEmptyLines(CodeBlockAnalysisContext context)
        {
            // Get the block
            MethodDeclarationSyntax methodDeclarationSyntax = context.CodeBlock as MethodDeclarationSyntax;
            if (methodDeclarationSyntax == null)
                return;
            IEnumerable<BlockSyntax> blocks = methodDeclarationSyntax.DescendantNodes().Where(node => SyntaxKind.Block == node.Kind()).OfType<BlockSyntax>();

            // Iterate through each block
            foreach (BlockSyntax block in blocks)
            {
                // Get a list of node, single line comment and end of line...
                List<Tuple<object, SyntaxKind>> blockObjectList = new List<Tuple<object, SyntaxKind>>();
                foreach (SyntaxNode node in block.ChildNodes())
                {
                    bool isNodeAdded = false;
                    SyntaxTrivia[] allTrivias = node.DescendantTrivia().Where(trivia => SyntaxKind.SingleLineCommentTrivia == trivia.Kind() || SyntaxKind.EndOfLineTrivia == trivia.Kind()).ToArray();
                    foreach (SyntaxTrivia trivia in allTrivias)
                    {
                        if (trivia.SpanStart < node.SpanStart)
                        {
                            blockObjectList.Add(Tuple.Create<object, SyntaxKind>(trivia, trivia.Kind()));
                        }
                        else
                        {
                            if (!isNodeAdded)
                            {
                                blockObjectList.Add(Tuple.Create<object, SyntaxKind>(node, SyntaxKind.None));
                                isNodeAdded = true;
                            }
                            blockObjectList.Add(Tuple.Create<object, SyntaxKind>(trivia, trivia.Kind()));
                        }
                    }
                }

                // Trim the block object list
                // Remove end of line after a "sentence"
                List<Tuple<object, SyntaxKind>> trimmedBlockObjectList = new List<Tuple<object, SyntaxKind>>();
                for (int i = 0; i < blockObjectList.Count() - 1; i++)
                {
                    if (blockObjectList[i].Item2 != SyntaxKind.EndOfLineTrivia)
                    {
                        if (blockObjectList[i + 1].Item2 == SyntaxKind.EndOfLineTrivia)
                        {
                            trimmedBlockObjectList.Add(blockObjectList[i]);
                            i++;
                        }
                        continue;
                    }
                    trimmedBlockObjectList.Add(blockObjectList[i]);
                }

                //// Cannot start with empty lines
                for (int i = 0; i < trimmedBlockObjectList.Count(); i++)
                {
                    if (blockObjectList[i].Item2 == SyntaxKind.EndOfLineTrivia)
                    {
                        context.ReportDiagnostic(Diagnostic.Create(BTAnalyzer.Rule, ((SyntaxTrivia)trimmedBlockObjectList[i].Item1).GetLocation(), ErrorCode.ExtraLine));
                    }
                    else
                        break;
                }

                // Statements should have comments before
                for (int i = 0; i < trimmedBlockObjectList.Count(); i++)
                {
                    while (i < trimmedBlockObjectList.Count() && trimmedBlockObjectList[i].Item2 != SyntaxKind.None)
                        i++;

                    if (i < trimmedBlockObjectList.Count())
                    {
                        if ((i - 1 < 0 || trimmedBlockObjectList[i - 1].Item2 != SyntaxKind.SingleLineCommentTrivia) && (trimmedBlockObjectList.Count > 1))
                            context.ReportDiagnostic(Diagnostic.Create(BTAnalyzer.Rule, ((SyntaxNode)trimmedBlockObjectList[i].Item1).GetLocation(), ErrorCode.MissingComment));
                    }

                    while (i < trimmedBlockObjectList.Count() && trimmedBlockObjectList[i].Item2 == SyntaxKind.None)
                        i++;

                    if (i < trimmedBlockObjectList.Count() && trimmedBlockObjectList[i].Item2 != SyntaxKind.EndOfLineTrivia)
                        context.ReportDiagnostic(Diagnostic.Create(BTAnalyzer.Rule, ((SyntaxTrivia)trimmedBlockObjectList[i].Item1).GetLocation(), ErrorCode.MissingEmptyLine));
                }

                // Cannot end with empty lines
                IEnumerable<SyntaxTrivia> triviaBeforeClosingBrackets = block.CloseBraceToken.LeadingTrivia;
                if (triviaBeforeClosingBrackets.Count() > 1)
                    context.ReportDiagnostic(Diagnostic.Create(BTAnalyzer.Rule, block.CloseBraceToken.GetLocation(), ErrorCode.UnexpectedComponentsBeforeClosingBracket));
            }
        }

        /// <summary>
        /// Analyzes class comments.
        /// </summary>
        /// <param name="context">Context.</param>
        private static void AnalyzeSummaryComments(SyntaxNodeAnalysisContext context)
        {
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
                BTAnalyzer.ReportDiagnostic(context, declarationNode.GetFirstToken().GetLocation(), ErrorCode.InvalideXmlComment);
                return;
            }

            // Get the only single line documentation comment trivia
            SyntaxTrivia singleDocCommentTrivia = singleDocumentationCommentTrivias.First();
            List<string> messageList = new List<string>();
            List<Location> locationList = new List<Location>();
            List<string> paramCommentNameList = new List<string>();
            int returnCount = 0;
            if (!BTAnalyzer.CheckDocumentationCommentTrivia(singleDocCommentTrivia, ref messageList, ref locationList, ref paramCommentNameList, ref returnCount))
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

        private static bool CheckDocumentationCommentTrivia(SyntaxTrivia commentTria, ref List<string> messageList, ref List<Location> locationList, ref List<string> paramCommentNameList, ref int returnCount)
        {
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
                messageList.Add(ErrorCode.InvalideXmlComment);
                return false;
            }

            // Check each XML element
            Location location = commentTria.GetLocation();
            string message = string.Empty;
            foreach (XmlElementSyntax xmlElement in xmlElements)
            {
                switch (xmlElement.StartTag.Name.ToString())
                {
                    case "summary":
                    {
                        if (!BTAnalyzer.CheckSummaryElement(xmlElement, ref location, ref message))
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
                // Check summary element
                // Check param element
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

            // Check XML element text
            IEnumerable<SyntaxToken> xmlTextLiteralTokens = xmlElement.DescendantTokens().Where(token => SyntaxKind.XmlTextLiteralToken == token.Kind());
            foreach (SyntaxToken xmlTextLiteralToken in xmlTextLiteralTokens)
            {
                foreach (StringValidator.Validate validate in BTAnalyzer.ReturnTextValidators)
                {
                    if (!validate(xmlTextLiteralToken.Text, ref message))
                    {
                        location = xmlTextLiteralToken.GetLocation();
                        return false;
                    }
                }
            }

            return true;

        }

        /// <summary>
        /// Checks param element.
        /// </summary>
        /// <param name="xmlElement">XML element.</param>
        /// <param name="location">Location.</param>
        /// <param name="message">Message.</param>
        /// <returns></returns>
        private static bool CheckParamElement(XmlElementSyntax xmlElement, ref Location location, ref string message, ref List<string> paramCommentNameList)
        {
            // Check tags
            if (!BTAnalyzer.CheckXmlTags(xmlElement, ref location, ref message))
                return false;

            // Add param name to the list
            XmlNameAttributeSyntax xmlNameAttribute = xmlElement.StartTag.Attributes.Where(attr => SyntaxKind.XmlNameAttribute == attr.Kind()).FirstOrDefault() as XmlNameAttributeSyntax;
            if (null == xmlNameAttribute)
            {
                location = xmlElement.GetLocation();
                message = ErrorCode.MissingNameAttribute;
                return false;
            }
            paramCommentNameList.Add(xmlNameAttribute.Identifier.ToString());

            // Check XML element text
            IEnumerable<SyntaxToken> xmlTextLiteralTokens = xmlElement.DescendantTokens().Where(token => SyntaxKind.XmlTextLiteralToken == token.Kind());
            foreach (SyntaxToken xmlTextLiteralToken in xmlTextLiteralTokens)
            {
                foreach (StringValidator.Validate validate in BTAnalyzer.ParamTextValidators)
                {
                    if (!validate(xmlTextLiteralToken.Text, ref message))
                    {
                        location = xmlTextLiteralToken.GetLocation();
                        return false;
                    }
                }
            }

            return true;
        }


        /// <summary>
        /// Checks summary element.
        /// </summary>
        /// <param name="xmlElement">The XML element.</param>
        /// <param name="location">Location.</param>
        /// <param name="message">Message.</param>
        /// <returns></returns>
        private static bool CheckSummaryElement(XmlElementSyntax xmlElement, ref Location location, ref string message)
        {
            // Check xml start/end tags
            if (!BTAnalyzer.CheckXmlTags(xmlElement, ref location, ref message))
                return false;

            // Check XML element text
            IEnumerable<SyntaxToken> xmlTextLiteralTokens = xmlElement.DescendantTokens().Where(token => SyntaxKind.XmlTextLiteralToken == token.Kind());
            foreach (SyntaxToken xmlTextLiteralToken in xmlTextLiteralTokens)
            {
                location = xmlTextLiteralToken.GetLocation();

                // Continue if it is just a white space
                if (" " == xmlTextLiteralToken.Text)
                    continue;

                // Validate text
                foreach (StringValidator.Validate validate in BTAnalyzer.SummaryTextValidators)
                {
                    if (!validate(xmlTextLiteralToken.Text, ref message))
                    {
                        location = xmlTextLiteralToken.GetLocation();
                        return false;
                    }
                }
            }

            return true;
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
                message = ErrorCode.InvalideXmlComment;
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

        private static void ReportDiagnostic(SyntaxNodeAnalysisContext context, Location location, string message)
        {
            var diagnostic = Diagnostic.Create(Rule, location, message);
            context.ReportDiagnostic(diagnostic);
        }

        private static bool IsValidXmlCommentTag(string tagName)
        {
            return "summary" == tagName || "param" == tagName || "returns" == tagName;
        }

        private static bool IsEqualFamilyExpression(SyntaxKind kind)
        {
            return kind == SyntaxKind.EqualsExpression
                || kind == SyntaxKind.NotEqualsExpression
                || kind == SyntaxKind.LessThanExpression
                || kind == SyntaxKind.LessThanOrEqualExpression
                || kind == SyntaxKind.GreaterThanExpression
            || kind == SyntaxKind.GreaterThanOrEqualExpression;
        }
    }
}

