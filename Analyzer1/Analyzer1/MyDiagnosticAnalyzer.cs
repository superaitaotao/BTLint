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
            context.RegisterSyntaxNodeAction(AnalyzeClassComments, SyntaxKind.ClassDeclaration);
            context.RegisterSyntaxNodeAction(AnalyzeMethodComments, SyntaxKind.MethodDeclaration);
        }

        /// <summary>
        /// Analyzes comments before method declaration.
        /// </summary>
        /// <param name="context">Analysis context.</param>
        private static void AnalyzeMethodComments(SyntaxNodeAnalysisContext context)
        {
            MethodDeclarationSyntax methodDeclarationSyntax = (MethodDeclarationSyntax)context.Node;
            SyntaxTrivia? singleLineDocumentationTrivia = null;
            foreach (SyntaxToken syntaxToken in methodDeclarationSyntax.DescendantTokens())
            {
                string message = string.Empty;
                foreach (var trivia in syntaxToken.GetAllTrivia())
                {
                    if (SyntaxKind.SingleLineDocumentationCommentTrivia == trivia.Kind())
                        singleLineDocumentationTrivia = trivia;
                    if (CommentAnalyzer.IsComment(trivia.Kind()))
                    {
                        if (!CommentAnalyzer.CheckCommentTrivia(trivia.ToFullString(), ref message))
                            CommentAnalyzer.ReportDiagnostic(context, trivia.GetLocation(), message);
                    }
                }
            }

            Tuple<XmlElementSyntax, string>[] paramComments = CommentAnalyzer.GetParamComments(singleLineDocumentationTrivia.Value).ToArray();

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

        /// <summary>
        /// Analyzes class comments.
        /// </summary>
        /// <param name="context">Context.</param>
        private static void AnalyzeClassComments(SyntaxNodeAnalysisContext context)
        {
            ClassDeclarationSyntax classDeclarationSyntax = (ClassDeclarationSyntax)context.Node;
            foreach (SyntaxToken syntaxToken in classDeclarationSyntax.DescendantTokens())
            {
                string message = string.Empty;
                foreach (var trivia in syntaxToken.GetAllTrivia())
                {
                    if (CommentAnalyzer.IsComment(trivia.Kind()))
                    {
                        if (!CommentAnalyzer.CheckCommentTrivia(trivia.ToFullString(), ref message))
                            CommentAnalyzer.ReportDiagnostic(context, trivia.GetLocation(), message);
                    }
                }
            }
        }

        private static bool CheckCommentTrivia(string classComment, ref string message)
        {
            foreach (string line in classComment.Split(new string[] { "\r\n" }, StringSplitOptions.None))
            {
                string tLine = line.Trim();
                if (String.IsNullOrWhiteSpace(tLine))
                    return true;

                if (tLine.Contains("  ") || tLine.Contains("   "))
                {
                    message = ErrorCode.MultipleSpacesDetected;
                    return false;
                }

                if (!tLine.EndsWith(".") && !tLine.Contains("summary") && !tLine.Contains("</param>"))
                {
                    message = ErrorCode.ClassCommentEndDot;
                    return false;
                }

                if (!tLine.StartsWith("///"))
                {
                    message = ErrorCode.ClassCommentStart;
                    return false;
                }

                if (!tLine.StartsWith("/// "))
                {
                    message = ErrorCode.MissingSpace;
                    return false;
                }
            }
            return true;
        }


        private static void ReportDiagnostic(SyntaxNodeAnalysisContext context, Location location, string message)
        {
            var diagnostic = Diagnostic.Create(ClassCommentRule, location, message);
            context.ReportDiagnostic(diagnostic);
        }

        private static bool IsComment(SyntaxKind kind)
        {
            return SyntaxKind.SingleLineDocumentationCommentTrivia == kind || SyntaxKind.SingleLineCommentTrivia == kind;
        }
    }
}

