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
    public class CommentAnalyzer : DiagnosticAnalyzer
    {
        /// <summary>
        /// Diagnostic ID.
        /// </summary>
        public const string DiagnosticId = nameof(CommentAnalyzer);

        /// <summary>
        /// Category.
        /// </summary>
        private const string Category = "CommentFormatting";

        /// <summary>
        /// Class comment rule.
        /// </summary>
        private static DiagnosticDescriptor ClassCommentRule = new DiagnosticDescriptor(CommentAnalyzer.DiagnosticId, "Class comment error", "{0}", Category, DiagnosticSeverity.Warning, true);

        /// <summary>
        /// Supported Diagnostics.
        /// </summary>
        public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics { get { return ImmutableArray.Create(ClassCommentRule); } }

        /// <summary>
        /// Initializes.
        /// </summary>
        /// <param name="context"></param>
        public override void Initialize(AnalysisContext context)
        {
            context.RegisterSyntaxNodeAction(AnalyzeMethodDeclaration, SyntaxKind.MethodDeclaration);
            context.RegisterSyntaxNodeAction(AnalyzeClassDeclaration, SyntaxKind.ClassDeclaration);
        }

        private static void AnalyzeMethodDeclaration(SyntaxNodeAnalysisContext context)
        {
            CommentAnalyzer.AnalyzeSummaryComments(context);
        }

        private static void AnalyzeClassDeclaration(SyntaxNodeAnalysisContext context)
        {
            CommentAnalyzer.AnalyzeSummaryComments(context);
        }

        /// <summary>
        /// Analyzes comments before method declaration.
        /// </summary>
        /// <param name="context">Analysis context.</param>
        private static void AnalyzeParamComments(SyntaxNodeAnalysisContext context)
        {
            MethodDeclarationSyntax methodDeclarationSyntax = (MethodDeclarationSyntax)context.Node;
            SyntaxToken firstToken = methodDeclarationSyntax.DescendantTokens().FirstOrDefault();
            SyntaxTrivia singleLineDocumentationTrivia = firstToken.GetAllTrivia().FirstOrDefault(trivia => SyntaxKind.SingleLineCommentTrivia == trivia.Kind());

            Tuple<XmlElementSyntax, string>[] paramComments = CommentAnalyzer.GetParamComments(singleLineDocumentationTrivia).ToArray();
            if (paramComments.Count() != methodDeclarationSyntax.ParameterList.Parameters.Count())
                CommentAnalyzer.ReportDiagnostic(context, methodDeclarationSyntax.ParameterList.GetLocation(), ErrorCode.IncompatibleParamComments);

            foreach (var param in methodDeclarationSyntax.ParameterList.Parameters)
            {
                var paramComment = paramComments.Where(comment => comment.Item2 == param.Identifier.ToString()).FirstOrDefault();
                if (null == paramComment)
                    CommentAnalyzer.ReportDiagnostic(context, param.GetLocation(), ErrorCode.MissingParamComment);
            }
        }

        /// <summary>
        /// Gets parameter comments in a single line documentation trivia.
        /// </summary>
        /// <param name="singleLineDocumentationTrivia">Single line documentation trivia.</param>
        /// <returns>A list of parameter comment syntax.</returns>
        private static List<Tuple<XmlElementSyntax, string>> GetParamComments(SyntaxTrivia singleLineDocumentationTrivia)
        {
            List<Tuple<XmlElementSyntax, string>> paramCommentList = new List<Tuple<XmlElementSyntax, string>>();
            if (!singleLineDocumentationTrivia.HasStructure)
                return paramCommentList;

            SyntaxNode commentNode = singleLineDocumentationTrivia.GetStructure();
            foreach (SyntaxNode node in commentNode.ChildNodes())
            {
                XmlElementSyntax xmlElement = node as XmlElementSyntax;
                if (xmlElement != null && "param" == xmlElement.StartTag.Name.LocalName.ToString())
                {
                    XmlNameAttributeSyntax xmlNameAttribute = xmlElement.StartTag.Attributes.FirstOrDefault() as XmlNameAttributeSyntax;
                    paramCommentList.Add(Tuple.Create(xmlElement, xmlNameAttribute.Identifier.ToString()));
                }
            }
            return paramCommentList;
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
            StringValidator.FirstWordInSForm,
        };

        private static readonly StringValidator.Validate[] ReturnTextValidators = new StringValidator.Validate[]
        {
            StringValidator.CommentNotEmpty,
            StringValidator.EndWithDot,
            StringValidator.NoMultipleSpace,
            StringValidator.NotStartsWithSpace,
            StringValidator.StartWithCapitalLetter,
            StringValidator.FirstWordInSForm,
        };

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
                CommentAnalyzer.ReportDiagnostic(context, singleLineCommentTrivias.First().GetLocation(), ErrorCode.ClassCommentStart);
                return;
            }

            // Only 1 single documentation line comment
            IEnumerable<SyntaxTrivia> singleDocumentationCommentTrivias = firstToken?.GetAllTrivia().Where(trivia => SyntaxKind.SingleLineDocumentationCommentTrivia == trivia.Kind());
            if (0 == singleDocumentationCommentTrivias.Count())
            {
                CommentAnalyzer.ReportDiagnostic(context, declarationNode.GetFirstToken().GetLocation(), ErrorCode.MissingComment);
                return;
            }
            else if (1 < singleDocumentationCommentTrivias.Count())
            {
                CommentAnalyzer.ReportDiagnostic(context, declarationNode.GetFirstToken().GetLocation(), ErrorCode.InvalideXmlComment);
                return;
            }

            // Get the only single line documentation comment trivia
            SyntaxTrivia singleDocCommentTrivia = singleDocumentationCommentTrivias.First();
            List<string> messageList = new List<string>();
            List<Location> locationList = new List<Location>();
            List<string> paramCommentNameList = new List<string>();
            int returnCount = 0;
            if (!CommentAnalyzer.CheckDocumentationCommentTrivia(singleDocCommentTrivia, ref messageList, ref locationList, ref paramCommentNameList, ref returnCount))
            {
                for (int i = 0; i < messageList.Count(); i++)
                    CommentAnalyzer.ReportDiagnostic(context, locationList[i], messageList[i]);
            }

            // Check method declaration
            // Check parameter comments
            MethodDeclarationSyntax methodDeclarationSyntax = declarationNode as MethodDeclarationSyntax;
            if (null == methodDeclarationSyntax)
                return;

            // Check parameter comment count
            IEnumerable<ParameterSyntax> parameters = methodDeclarationSyntax.ParameterList.Parameters;
            if (paramCommentNameList.Count() != parameters.Count())
                CommentAnalyzer.ReportDiagnostic(context, methodDeclarationSyntax.ParameterList.GetLocation(), ErrorCode.IncompatibleParamComments);

            // Check that all parameters have comment
            foreach (ParameterSyntax parameter in parameters)
            {
                if (!paramCommentNameList.Any(p => p == parameter.Identifier.Text))
                {
                    CommentAnalyzer.ReportDiagnostic(context, parameter.GetLocation(), ErrorCode.MissingParamComment);
                    break;
                }
            }

            // Check number of returns comments
            if ("void" == methodDeclarationSyntax.ReturnType.ToString())
            {
                if (0 < returnCount)
                {
                    CommentAnalyzer.ReportDiagnostic(context, methodDeclarationSyntax.ReturnType.GetLocation(), ErrorCode.IncorrectReturnComment);
                    return;
                }
            }
            else
            {
                if (1 != returnCount)
                {
                    CommentAnalyzer.ReportDiagnostic(context, methodDeclarationSyntax.ReturnType.GetLocation(), ErrorCode.IncorrectReturnComment);
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
                        if (!CommentAnalyzer.CheckSummaryElement(xmlElement, ref location, ref message))
                        {
                            result = false;
                            messageList.Add(message);
                            locationList.Add(location);
                        }
                        break;
                    }

                    case "param":
                    {
                        if (!CommentAnalyzer.CheckParamElement(xmlElement, ref location, ref message, ref paramCommentNameList))
                        {
                            result = false;
                            messageList.Add(message);
                            locationList.Add(location);
                        }
                        break;
                    }

                    case "returns":
                    {
                        if (!CommentAnalyzer.CheckReturnElement(xmlElement, ref location, ref message, ref returnCount))
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
            if (!CommentAnalyzer.CheckXmlTags(xmlElement, ref location, ref message))
                return false;

            // Increment number of <returns>
            returnCount++;

            // Check XML element text
            IEnumerable<SyntaxToken> xmlTextLiteralTokens = xmlElement.DescendantTokens().Where(token => SyntaxKind.XmlTextLiteralToken == token.Kind());
            foreach (SyntaxToken xmlTextLiteralToken in xmlTextLiteralTokens)
            {
                foreach (StringValidator.Validate validate in CommentAnalyzer.ReturnTextValidators)
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
            if (!CommentAnalyzer.CheckXmlTags(xmlElement, ref location, ref message))
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
                foreach (StringValidator.Validate validate in CommentAnalyzer.ParamTextValidators)
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
            if (!CommentAnalyzer.CheckXmlTags(xmlElement, ref location, ref message))
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
                foreach (StringValidator.Validate validate in CommentAnalyzer.SummaryTextValidators)
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
            if (!CommentAnalyzer.IsValidXmlCommentTag(startTag) || !CommentAnalyzer.IsValidXmlCommentTag(endTag))
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
            var diagnostic = Diagnostic.Create(ClassCommentRule, location, message);
            context.ReportDiagnostic(diagnostic);
        }

        private static bool IsValidXmlCommentTag(string tagName)
        {
            return "summary" == tagName || "param" == tagName || "returns" == tagName;
        }
    }
}

