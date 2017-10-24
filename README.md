# Spending too much time on formatting issues on Code Review?
# BTLint is here to save your precious time :)

## BTLint: A simple tool that highlights some formatting issues in C# code.
BTLint only covers those issues that are not handled by default formatting tools (Ctrl+K, Ctrl+D) in Visual Studio.
BTLint also provides quick fixes to some formatting issues found. (FIX)

List of formatting issues covered:

 * Block
    * Single line block should avoid bracket (FIX)
 * Logical Expression
    * Constant should be on the left (FIX)
    * Should use full parenthization (FIX) 
 * Class comments / Method comments
    * Should be there
    * Should have comments for all parameters
    * Should have comment for return (if not null)
    * Should start with ///
    * Should end with . 
    * Should not have multiple spaces
    * Should have space after ///
    * Should not correct xml tags (summary, param, return ..)
    * Should not have extra space
    * Should start with capital letter
    * First word (verb) should ends with -s -es
 * Comments inside methods
    * Should not have extra space
    * Should have empty space after each section
    * Should start with //
    * First word (verb) should not end with -s -es
    * First letter should be capital
