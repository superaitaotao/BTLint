namespace BTAnalyzer
{
    public class ErrorCode
    {
        public static readonly string ClassCommentStart = "Must start with ///";
        public static readonly string ClassCommentEndDot = "Missing '.' at the end";
        public static readonly string MultipleSpacesDetected = "Multiple spaces detected";
        public static readonly string MissingSpace = "Missing space";
        public static readonly string IncompatibleParamComments = "Incompatible parameter comments";
        internal static readonly string MissingParamComment = "Missing parameter comment";
        internal static readonly string MissingComment = "Missing comment";
        internal static readonly string MissingXmlEndTag = "Missing XML start tag";
        internal static readonly string MissingXmlStartTag = "Missing XML end tag";
        internal static readonly string ErrorsInComment = "Errors in XML comment";
        internal static readonly string InvalidXmlTag = "Invalid XML tag";
        internal static readonly string MissingNameAttribute = "Missing name attribute";
        internal static readonly string ExtraSpace = "Extra Space";
        internal static readonly string MustStartWithCapitalLetter = "Capital letter first";
        internal static readonly string FirstWordMustBeSForm = "First verb may be in -s -es form";
        internal static readonly string IncorrectReturnComment = "Incorrect return comment";
        internal static readonly string CommentNotEndWithDot = "Should not end with .";
        internal static readonly string FirstWordNotInSForm = "First verb may not be in -s -es form";
        internal static readonly string MustStartWithTwoSlashes = "Must start with //";
        internal static readonly string ExtraLine = "Extra empty line space";
        internal static readonly string MissingEmptyLine = "Missing empty line space";
        internal static readonly string UnexpectedComponentsBeforeClosingBracket = "Unexpected components before closing bracket";
        internal static readonly string InvalidExpression = "Invalid expression";
        internal static readonly string ConstantOnLeft = "Constant should be on the left hand side";
        internal static readonly string MissingFullParenthization = "Use full parenthization in logical expressions";
        internal static readonly string UnnecessaryBlock = "Avoid One Line Bracketing";
    }
}
