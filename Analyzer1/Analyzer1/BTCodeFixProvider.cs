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

namespace BTAnalyzer
{
    [ExportCodeFixProvider(LanguageNames.CSharp, Name = nameof(BTCodeFixProvider)), Shared]
    public class BTCodeFixProvider : CodeFixProvider
    {
        private const string MAKE_CONSTANT_LEFT = "Make constant left";
        private const string ADD_FULL_PARENTHESIS = "Add full parenthesis";

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
            SyntaxNode equalStatement = root.FindNode(diagnosticSpan);

            // Register a code action that will invoke the fix
            context.RegisterCodeFix(
                CodeAction.Create(
                    title: MAKE_CONSTANT_LEFT,
                    createChangedDocument: c => MakeConstantLeft(context.Document, equalStatement, c)
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
            try
            {
                newRoot = oldRoot.ReplaceNodes(childNodes, (original, _) => original == childNodes[0] ? childNodes[1] : childNodes[0]);
            }
            catch (Exception e)
            {

            }

            // Return document with transformed tree
            return document.WithSyntaxRoot(newRoot);
        }

        private static async Task<Document> FixFullParenthesisError(Document document, SyntaxNode node, CancellationToken cancellationToken)
        {
            // Get child nodes
            SyntaxNode[] childNodes = node.ChildNodes().ToArray();
            if (2 != childNodes.Count())
                return null;

            // Replace the old local declaration with the new local declaration
            var oldRoot = await document.GetSyntaxRootAsync(cancellationToken);
            SyntaxNode newRoot = null;
            try
            {
                newRoot = oldRoot.ReplaceNodes(childNodes, (original, _) => original == childNodes[0] ? childNodes[1] : childNodes[0]);
            }
            catch (Exception e)
            {

            }

            // Return document with transformed tree
            return document.WithSyntaxRoot(newRoot);
        }

        //private async Task<Solution> MakeUppercaseAsync(Document document, TypeDeclarationSyntax typeDecl, CancellationToken cancellationToken)
        //{
        //    // Compute new uppercase name
        //    var identifierToken = typeDecl.Identifier;
        //    var newName = identifierToken.Text.ToUpperInvariant();

        //    // Get the symbol representing the type to be renamed
        //    var semanticModel = await document.GetSemanticModelAsync(cancellationToken);
        //    var typeSymbol = semanticModel.GetDeclaredSymbol(typeDecl, cancellationToken);

        //    // Produce a new solution that has all references to that type renamed, including the declaration
        //    var originalSolution = document.Project.Solution;
        //    var optionSet = originalSolution.Workspace.Options;
        //    var newSolution = await Renamer.RenameSymbolAsync(document.Project.Solution, typeSymbol, newName, optionSet, cancellationToken).ConfigureAwait(false);

        //    // Return the new solution with the now-uppercase type name
        //    return newSolution;
        //}
    }
}