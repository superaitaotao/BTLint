using System;
using System.Collections.Immutable;
using System.Composition;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CodeFixes;
using Microsoft.CodeAnalysis.CodeActions;
using Microsoft.CodeAnalysis.Text;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using System.Collections.Generic;

namespace BTAnalyzer
{
    [ExportCodeFixProvider(LanguageNames.CSharp, Name = nameof(BTCodeFixProvider)), Shared]
    public class BTCodeFixProvider : CodeFixProvider
    {
        private const string MAKE_CONSTANT_LEFT = "Make constant left";
        private const string ADD_FULL_PARENTHESIS = "Add full parenthesis";
        private const string REMOVE_SINGLE_LINE_BRACKET = "Remove single line bracket";

        public sealed override ImmutableArray<string> FixableDiagnosticIds
        {
            get { return ImmutableArray.Create(BTAnalyzer.DiagnosticId); }
        }

        public sealed override FixAllProvider GetFixAllProvider()
        {
            // See https://github.com/dotnet/roslyn/blob/master/docs/analyzers/FixAllProvider.md for more information on Fix All Providers
            return WellKnownFixAllProviders.BatchFixer;
        }

        public sealed override async Task RegisterCodeFixesAsync(CodeFixContext context)
        {
            // Fix constant on left error
            if (await RegisterErrorFix(context, BTCodeFixProvider.MAKE_CONSTANT_LEFT, ErrorCode.ConstantOnLeft, BTCodeFixProvider.MakeConstantLeft))
                return;

            // Fix missing bracket error
            if (await RegisterErrorFix(context, BTCodeFixProvider.ADD_FULL_PARENTHESIS, ErrorCode.MissingFullParenthization, BTCodeFixProvider.FixFullParenthesisError))
                return;

            // Fix single line parenthesis
            if (await RegisterErrorFix(context, BTCodeFixProvider.REMOVE_SINGLE_LINE_BRACKET, ErrorCode.UnnecessaryBlock, BTCodeFixProvider.RemoveUnnecessaryBlock))
                return;
        }

        private async Task<bool> RegisterErrorFix(CodeFixContext context, string title, string errorMessage, Func<Document, SyntaxNode, CancellationToken, Task<Document>> func)
        {
            // Get root
            var root = await context.Document.GetSyntaxRootAsync(context.CancellationToken).ConfigureAwait(false);

            // Fix constant on left diagnostic
            Diagnostic diagnostic = context.Diagnostics.Where(diag => diag.GetMessage().Equals(errorMessage)).FirstOrDefault();

            // Return early
            if (null == diagnostic)
                return false;

            // Get diagnostic span
            TextSpan diagnosticSpan = diagnostic.Location.SourceSpan;

            // Find the type declaration identified by the diagnostic
            SyntaxNode node = root.FindNode(diagnosticSpan);

            // Register a code action that will invoke the fix
            context.RegisterCodeFix(
                CodeAction.Create(
                    title: title,
                    createChangedDocument: c => func(context.Document, node, c)
                    ),
                diagnostic);

            // Return true
            return true;
        }

        private static async Task<Document> MakeConstantLeft(Document document, SyntaxNode node, CancellationToken cancellationToken)
        {
            // Get child nodes
            SyntaxNode[] childNodes = node.ChildNodes().ToArray();
            if (2 != childNodes.Count())
                return null;
            // Replace the old local declaration with the new local declaration
            var oldRoot = await document.GetSyntaxRootAsync(cancellationToken);
            SyntaxNode newRoot = null;
            SyntaxNode newNode = null;
            try
            {
                newNode = node.ReplaceNodes(childNodes, (original, _) => original == childNodes[0] ? childNodes[1] : childNodes[0]);
                SyntaxToken token = newNode.ChildTokens().FirstOrDefault();
                if (null == token)
                    return null;

                if (BTCodeFixProvider.GreaterDictionary.ContainsKey(token.Kind()))
                    newNode = newNode.ReplaceToken(token, SyntaxFactory.Token(BTCodeFixProvider.GreaterDictionary[token.Kind()]));

                newRoot = oldRoot.ReplaceNode(node, newNode);
            }
            catch (Exception e)
            {

            }

            // Return document with transformed tree
            return document.WithSyntaxRoot(newRoot);
        }

        private static async Task<Document> FixFullParenthesisError(Document document, SyntaxNode node, CancellationToken cancellationToken)
        {
            // Get expression node
            ExpressionSyntax expression = node as ExpressionSyntax;
            if (null == expression)
                return null;

            // Create a parenthesized expression
            ParenthesizedExpressionSyntax parenthesizedExpressionSyntax = SyntaxFactory.ParenthesizedExpression(expression);

            // Replace the old local declaration with the new local declaration
            var oldRoot = await document.GetSyntaxRootAsync(cancellationToken);
            SyntaxNode newRoot = null;
            try
            {
                newRoot = oldRoot.ReplaceNode(node, parenthesizedExpressionSyntax);
            }
            catch (Exception e)
            {

            }

            // Return document with transformed tree
            return document.WithSyntaxRoot(newRoot);
        }


        private static async Task<Document> RemoveUnnecessaryBlock(Document document, SyntaxNode node, CancellationToken cancellationToken)
        {
            // Get expression node
            BlockSyntax block = node.ChildNodes().Where(nodee => SyntaxKind.Block == nodee.Kind()).OfType<BlockSyntax>().FirstOrDefault();
            if (null == block)
                return null;

            // Replace the old local declaration with the new local declaration
            SyntaxNode newNode = block.ChildNodes().FirstOrDefault();
            if (null == newNode)
                return null;

            var oldRoot = await document.GetSyntaxRootAsync(cancellationToken);
            SyntaxNode newRoot = null;
            try
            {
                newRoot = oldRoot.ReplaceNode(block, newNode);
            }
            catch (Exception e)
            {

            }

            // Return document with transformed tree
            return document.WithSyntaxRoot(newRoot);
        }

        private static readonly Dictionary<SyntaxKind, SyntaxKind> GreaterDictionary = new Dictionary<SyntaxKind, SyntaxKind>
        {
            { SyntaxKind.GreaterThanEqualsToken, SyntaxKind.LessThanEqualsToken },
            { SyntaxKind.LessThanEqualsToken, SyntaxKind.GreaterThanEqualsToken },
            { SyntaxKind.GreaterThanToken, SyntaxKind.LessThanToken },
            { SyntaxKind.LessThanToken, SyntaxKind.GreaterThanToken }
        };
    }
}