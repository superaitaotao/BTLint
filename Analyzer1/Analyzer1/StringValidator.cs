using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Analyzer1
{
    public static class StringValidator
    {

        public delegate bool Validate(string text, ref string message);

        public static bool CommentNotEmpty(string text, ref string message)
        {
            // Cannot be empty
            if (string.IsNullOrWhiteSpace(text))
            {
                message = ErrorCode.MissingComment;
                return false;

            }

            // Return true
            return true;
        }

        public static bool StartsWithSpace(string text, ref string message)
        {
            // Start with space
            if (!text.StartsWith(" "))
            {
                message = ErrorCode.MissingSpace;
                return false;
            }

            // Return true
            return true;
        }

        public static bool NotStartsWithSpace(string text, ref string message)
        {
            // Start with space
            if (text.StartsWith(" "))
            {
                message = ErrorCode.ExtraSpace;
                return false;
            }

            // Return true
            return true;
        }

        public static bool NoMultipleSpace(string text, ref string message)
        {
            // Must not have extra space
            if (text.Contains("  "))
            {
                message = ErrorCode.MultipleSpacesDetected;
                return false;
            }

            // Return true
            return true;
        }

        public static bool EndWithDot(string text, ref string message)
        {
            // Trim string
            string trimmedText = text.Trim();

            // End with dot
            if (!trimmedText.EndsWith("."))
            {
                message = ErrorCode.ClassCommentEndDot;
                return false;
            }

            // Return true
            return true;
        }

        public static bool NotEndWithDot(string text, ref string message)
        {
            // Trim string
            string trimmedText = text.Trim();

            // End with dot
            if (trimmedText.EndsWith("."))
            {
                message = ErrorCode.CommentNotEndWithDot;
                return false;
            }

            // Return true
            return true;
        }

        public static bool StartWithCapitalLetter(string text, ref string message)
        {
            // Trim string
            string trimmedText = text.Trim();

            // Start with a capital letter
            if (!char.IsUpper(trimmedText[0]))
            {
                message = ErrorCode.MustStartWithCapitalLetter;
                return false;
            }

            // Return true
            return true;
        }

        public static bool FirstWordInSForm(string text, ref string message)
        {
            // Trim string
            string trimmedText = text.Trim();

            // First word should be a verb with s or es
            if (!(trimmedText.Split(' ')[0].EndsWith("s") && (!trimmedText.Split(' ')[0].EndsWith("es"))))
            {
                message = ErrorCode.FirstWordMustBeSForm;
                return false;
            }

            // Return true
            return true;
        }

        public static bool FirstWordNotInSForm(string text, ref string message)
        {
            // Trim string
            string trimmedText = text.Trim();

            // First word should be a verb with s or es
            if ((trimmedText.Split(' ')[0].EndsWith("s") || (trimmedText.Split(' ')[0].EndsWith("es"))))
            {
                message = ErrorCode.FirstWordNotInSForm;
                return false;
            }

            // Return true
            return true;
        }

        public static bool StartWithTwoSlashes(string text, ref string message)
        {
            if(!text.StartsWith("//"))
            {
                message = ErrorCode.MustStartWithTwoSlashes;
                return false;
            }

            if(!text.StartsWith("// "))
            {
                message = ErrorCode.MissingSpace;
                return false;
            }

            // Return true
            return true;
        }
    }
}
