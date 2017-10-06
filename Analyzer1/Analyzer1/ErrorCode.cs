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
    }
}
