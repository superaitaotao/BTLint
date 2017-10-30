using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;

namespace BTAnalyzer
{
    public static class StringValidator
    {
        public struct Position
        {
            public static Position Origin = new Position(0, 0);
            public int Start;
            public int Len;
            public Position(int start, int len)
            {
                this.Start = start;
                this.Len = len;
            }
        }

        public delegate bool Validate(string text, ref string message, ref Position position);

        public static bool CommentNotEmpty(string text, ref string message, ref Position position)
        {
            // Cannot be empty
            if (string.IsNullOrWhiteSpace(text))
            {
                message = ErrorCode.MissingComment;
                position = new Position(0, 10);
                return false;
            }

            // Return true
            return true;
        }

        public static bool StartsWithSpace(string text, ref string message, ref Position position)
        {
            // Start with space
            if (!text.StartsWith(" "))
            {
                message = ErrorCode.MissingSpace;
                position = new Position(0, 2);
                return false;
            }

            // Return true
            return true;
        }

        public static bool NotStartsWithSpace(string text, ref string message, ref Position position)
        {
            // Start with space
            if (text.StartsWith(" "))
            {
                message = ErrorCode.ExtraSpace;
                position = new Position(0, 2);
                return false;
            }

            // Return true
            return true;
        }

        public static bool NoMultipleSpace(string text, ref string message, ref Position position)
        {
            // Must not have extra space
            if (text.Contains("  "))
            {
                message = ErrorCode.MultipleSpacesDetected;
                position = new Position(text.IndexOf("  "), 2);
                return false;
            }

            // Return true
            return true;
        }

        public static bool EndWithDot(string text, ref string message, ref Position position)
        {
            // Trim string
            string trimmedText = text.TrimEnd('\r','\n', '/', ' ');

            // End with dot
            if (!trimmedText.EndsWith("."))
            {
                message = ErrorCode.ClassCommentEndDot;
                position = new Position(trimmedText.Length - 3, 4);
                return false;
            }

            // Return true
            return true;
        }

        public static bool NotEndWithDot(string text, ref string message, ref Position position)
        {
            // Trim string
            string trimmedText = text.TrimEnd('\r','\n', '/', ' ');

            // End with dot
            if (trimmedText.EndsWith("."))
            {
                message = ErrorCode.CommentNotEndWithDot;
                position = new Position(trimmedText.Length - 3, 4);
                return false;
            }

            // Return true
            return true;
        }

        public static bool StartWithCapitalLetter(string text, ref string message, ref Position position)
        {
            // First word
            string firstWord = text.Trim().Split(' ')[0];

            // First letter should be upper
            if (!char.IsUpper(firstWord[0]))
            {
                message = ErrorCode.MustStartWithCapitalLetter;
                position = new Position(0, firstWord.Length);
                return false;
            }

            // Return true
            return true;
        }

        public static bool FirstWordInSForm(string text, ref string message, ref Position position)
        {
            // First word
            string firstWord = text.Trim().Split(' ')[0];

            // First word should be a verb with s or es
            if (!(firstWord.EndsWith("s") && !(firstWord.EndsWith("es"))))
            {
                message = ErrorCode.FirstWordMustBeSForm;
                position = new Position(0, firstWord.Length);
                return false;
            }

            // Return true
            return true;
        }

        public static bool FirstWordNotInSForm(string text, ref string message, ref Position position)
        {
            // First word
            string firstWord = text.Trim().Split(' ')[0];

            // First word should not be a verb with s or es
            if ((firstWord.EndsWith("s") || (firstWord.EndsWith("es"))))
            {
                message = ErrorCode.FirstWordNotInSForm;
                position = new Position(0, firstWord.Length);
                return false;
            }

            // Return true
            return true;
        }

        public static bool StartWithTwoSlashes(string text, ref string message, ref Position position)
        {
            if (!text.StartsWith("//"))
            {
                message = ErrorCode.MustStartWithTwoSlashes;
                position = new Position(0, 3);
                return false;
            }

            if (!text.StartsWith("// "))
            {
                message = ErrorCode.MissingSpace;
                position = new Position(0, 3);
                return false;
            }

            // Return true
            return true;
        }
    }
}
