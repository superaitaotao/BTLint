namespace Analyzer1
{
    public class ErrorCode
    {
        public static readonly string ClassCommentStart = "Must start with ///";
        public static readonly string ClassCommentEndDot = "Missing '.'";
        public static readonly string MultipleSpacesDetected = "Multiple spaces detected";
        public static readonly string MissingSpace = "Missing space";
        public static readonly string IncompatibleParamComments = "Incompatible parameter comments";
        internal static readonly string MissingParamComment = "Missing parameter comment";
        internal static readonly string MissingComment = "Missing comment";
        internal static readonly string MissingXmlEndTag = "Missing XML start tag";
        internal static readonly string MissingXmlStartTag = "Missing XML end tag";
        internal static readonly string InvalideXmlComment = "Invalid XML comment";
        internal static readonly string InvalidXmlTag = "Invalid XML tag";
        internal static readonly string MissingNameAttribute = "Missing name attribute";
        internal static readonly string ExtraSpace = "Extra Space";
        internal static readonly string MustStartWithCapitalLetter = "Capital letter first";
        internal static readonly string FirstWordMustBeSForm = "First verb should be in -s -es form";
        internal static readonly string IncorrectReturnComment = "Incorrect return comment";
        internal static readonly string CommentNotEndWithDot = "Should not end with .";
        internal static readonly string FirstWordNotInSForm = "First word cannot be in -s -es form";
        internal static readonly string MustStartWithTwoSlashes = "Must start with //";
        internal static readonly string ExtraLine = "Extra empty line space";
        internal static readonly string MissingEmptyLine = "Missing empty line space";
    }
}
