local lpeg = require "lpeglabel"
local C = lpeg.C
local Cb = lpeg.Cb
local Cg = lpeg.Cg
local Cmt = lpeg.Cmt
local P = lpeg.P
local S = lpeg.S
local V = lpeg.V
local alnum = lpeg.alnum
local alpha = lpeg.alpha
local digit = lpeg.digit
local space = lpeg.space
local xdigit = lpeg.xdigit

local errors = require "lua-parser.errors"
local newerr = errors.newerror
local throw = errors.throw

-- error message auxiliary functions

newerr("EndOfInput", "unexpected character(s), expected EOF")
newerr("InvalidStatement", "unexpected token, invalid start of statement")

newerr("EndIf", "expected 'end' to close the if statement")
newerr("ExprIf", "expected a condition after 'if'")
newerr("ThenIf", "expected 'then' after the condition")
newerr("ExprEIf", "expected a condition after 'elseif'")
newerr("ThenEIf", "expected 'then' after the condition")

newerr("EndDo", "expected 'end' to close the do block")
newerr("ExprWhile", "expected a condition after 'while'")
newerr("DoWhile", "expected 'do' after the condition")
newerr("EndWhile", "expected 'end' to close the while loop")
newerr("UntilRep", "expected 'until' at the end of the repeat loop")
newerr("ExprRep", "expected a conditions after 'until'")

newerr("ForRange", "expected a numeric or generic range after 'for'")
newerr("EndFor", "expected 'end' to close the for loop")
newerr("ExprFor1", "expected a starting expression for the numeric range")
newerr("CommaFor", "expected ',' to split the start and end of the range")
newerr("ExprFor2", "expected an ending expression for the numeric range")
newerr("ExprFor3", "expected a step expression for the numeric range after ','")
newerr("InFor", "expected '=' or 'in' after the variable(s)")
newerr("EListFor", "expected one or more expressions after 'in'")
newerr("DoFor", "expected 'do' after the range of the for loop")

newerr("DefLocal", "expected a function definition or assignment after local")
newerr("NameLFunc", "expected a function name after 'function'")
newerr("EListLAssign", "expected one or more expressions after '='")
newerr("EListAssign", "expected one or more expressions after '='")

newerr("FuncName", "expected a function name after 'function'")
newerr("OParenPList", "expected '(' for the parameter list")
newerr("CParenPList", "expected ')' to close the parameter list")
newerr("EndFunc", "expected 'end' to close the function body")
newerr("FuncParSpec", "expected a variable name or '...' after ','")

newerr("Label", "expected a label name after '::'")
newerr("CloseLabel", "expected '::' after the label")
newerr("Goto", "expected a label after 'goto'")
newerr("RetList", "expected an expression after ',' in the return statement")

newerr("VarList", "expected a variable name after ','")
newerr("ExprList", "expected an expression after ','")

newerr("OrExpr", "expected an expression after 'or'")
newerr("AndExpr", "expected an expression after 'and'")
newerr("RelExpr", "expected an expression after the relational operator")
newerr("BOrExpr", "expected an expression after '|'")
newerr("BXorExpr", "expected an expression after '~'")
newerr("BAndExpr", "expected an expression after '&'")
newerr("ShiftExpr", "expected an expression after the bit shift")
newerr("ConcatExpr", "expected an expression after '..'")
newerr("SumExpr", "expected an expression after the additive operator")
newerr("ProdExpr", "expected an expression after the multiplicative operator")
newerr("UnaryExpr", "expected an expression after the unary operator")
newerr("PowExpr", "expected an expression after '^'")

newerr("ExprParen", "expected an expression after '('")
newerr("CParenExpr", "expected ')' to close the expression")
newerr("NameIndex", "expected a field name after '.'")
newerr("ExprIndex", "expected an expression after '['")
newerr("CBracketIndex", "expected ']' to close the indexing expression")
newerr("NameMeth", "expected a method name after ':'")
newerr("MethArgs", "expected some arguments for the method call (or '()')")

newerr("ArgList", "expected an expression after ',' in the argument list")
newerr("CParenArgs", "expected ')' to close the argument list")

newerr("CBraceTable", "expected '}' to close the table constructor")
newerr("EqField", "expected '=' after the table key")
newerr("ExprField", "expected an expression after '='")
newerr("ExprFKey", "expected an expression after '[' for the table key")
newerr("CBracketFKey", "expected ']' to close the table key")

newerr("DigitHex", "expected one or more hexadecimal digits after '0x'")
newerr("DigitDeci", "expected one or more digits after the decimal point")
newerr("DigitExpo", "expected one or more digits for the exponent")

newerr("Quote", "unclosed string")
newerr("HexEsc", "expected exactly two hexadecimal digits after '\\x'")
newerr("OBraceUEsc", "expected '{' after '\\u'")
newerr("DigitUEsc", "expected one or more hexadecimal digits for the UTF-8 code point")
newerr("CBraceUEsc", "expected '}' after the code point")
newerr("InvalidQuoteEscape", "invalid escape sequence")
newerr("CloseLStr", "unclosed long string")

local function expect (patt, label)
  return patt + throw(label)
end

-- regular combinators and auxiliary functions

local function kw (w) return P(w) * -V"IdentRest" end
local function Vs (n) return V(n) * V"Skip" end
local function sV (n) return V"Skip" * V(n) end
local function sVs (n) return sV(n) * V"Skip" end

local function sepBy (patt, sepname, label)
  local other = label==nil and patt or expect(patt, label)
  return patt * (sVs(sepname) * other)^0
end

local function commaSep (patt, label)
  return sepBy(patt, "Comma", label)
end

local grammar = {
  Lua      = V"Shebang"^-1 * V"Block" * expect(P(-1), "EndOfInput");
  Shebang  = P"#!" * (P(1) - P"\n")^0;

  Block       = V"Skip" * Vs"Statement"^0 * Vs"RetStat"^-1;
  Statement   = V"IfStat"
              + V"DoStat"
              + V"WhileStat"
              + V"RepeatStat"
              + V"ForStat"
              + V"LocalStat"
              + V"FuncStat"
              + V"BreakStat"
              + V"LabelStat"
              + V"GoToStat"
              + V"CallStat"
              + V"Assignment"
              + V"Semicolon"
              + -V"BlockEnd" * throw("InvalidStatement");
  BlockEnd    = P"return" + "end" + "elseif" + "else" + "until" + -1;

  -- if ... then ... elseif ... then ... else ... end
  IfStat     = V"IfPart"
             * V"ElseIfPart"^0
             * V"ElsePart"^-1
             * expect(V"EndCmd", "EndIf");
  IfPart     = Vs"IfCmd"
             * expect(Vs"Expression", "ExprIf")
             * expect(V"ThenCmd", "ThenIf")
             * V"Block";
  ElseIfPart = Vs"ElIfCmd"
             * expect(Vs"Expression", "ExprEIf")
             * expect(V"ThenCmd", "ThenEIf")
             * V"Block";
  ElsePart   = V"ElseCmd"
             * V"Block";

  -- do ... end
  DoStat = V"DoCmd"
         * V"Block"
         * expect(V"EndCmd", "EndDo");

  -- while ... do ... end
  WhileStat = Vs"WhileCmd"
            * expect(Vs"Expression", "ExprWhile")
            * V"WhileBody";
  WhileBody = expect(V"DoCmd", "DoWhile")
            * V"Block"
            * expect(V"EndCmd", "EndWhile");

  -- repeat ... until
  RepeatStat = V"RepeatCmd"
             * V"Block"
             * expect(Vs"UntilCmd", "UntilRep")
             * expect(V"Expression", "ExprRep");

  -- for ... do ... end
  ForStat     = Vs"ForCmd"
              * expect(Vs"ForNumSpec" + Vs"ForInSpec", "ForRange")
              * V"ForBody";
  ForInSpec   = Vs"NameList"
              * expect(Vs"InCmd", "InFor")
              * expect(V"ExprList", "EListFor");
  ForNumSpec  = Vs"Name"
              * Vs"AssignOp"
              * V"ForNumRange";
  ForNumRange = expect(Vs"Expression", "ExprFor1")
              * expect(Vs"Comma", "CommaFor")
              * expect(V"Expression", "ExprFor2")
              * (sVs"Comma" * expect(V"Expression", "ExprFor3"))^-1;
  ForBody     = expect(V"DoCmd", "DoFor")
              * V"Block"
              * expect(V"EndCmd", "EndFor");

  -- local ...
  LocalStat = Vs"LocalCmd" * expect(V"LocalFunc" + V"LocalVar", "DefLocal");
  LocalFunc = Vs"FuncCmd" * expect(Vs"Name", "NameLFunc") * V"FuncBody";
  LocalVar  = V"NameList" * ( sVs"AssignOp" * expect(V"ExprList", "EListLAssign")
                            + P(true) -- or empty
                            );

  -- ... = ...
  Assignment  = Vs"VarList"
              * Vs"AssignOp"
              * expect(V"ExprList", "EListAssign");

  -- function ... (...) ... end
  FuncStat    = Vs"FuncCmd" * expect(Vs"FuncName", "FuncName") * V"FuncBody";
  FuncName    = V"Name"
              * (sV"FieldIndex")^0
              * (sV"MethodIndex")^-1;
  FuncBody    = V"FuncParams"
              * V"Block"
              * expect(V"EndCmd", "EndFunc");
  FuncParams  = expect(Vs"ParenOpen", "OParenPList")
              * Vs"FuncParSpec"
              * expect(V"ParenClose", "CParenPList");
  FuncParSpec = V"FuncParName" + V"Dots" + P(true);
  FuncParName = V"NameList"
              * (sVs"Comma" * expect(V"Dots", "FuncParSpec"))^-1;

  LabelStat  = Vs"LabelEdge"
             * expect(Vs"Identifier", "Label")
             * expect(V"LabelEdge", "CloseLabel");
  GoToStat   = Vs"GotoCmd" * expect(V"Identifier", "Goto");

  BreakStat  = V"BreakCmd";

  RetStat    = V"ReturnCmd"
             * (V"Skip" * commaSep(V"Expression", "RetList"))^-1
             * (sV"Semicolon")^-1;

  NameList = commaSep(V"Name");
  VarList  = commaSep(V"VarExpr", "VarList");
  ExprList = commaSep(V"Expression", "ExprList");

  CallStat = Cmt(V"SuffixedExpr", function(s, i, ...)
                                    return string.sub(s, i-1, i-1) == ")", ...
                                  end);
  VarExpr  = Cmt(V"SuffixedExpr", function(s, i, ...)
                                    return string.sub(s, i-1, i-1) ~= ")", ...
                                  end);

  Expression  = V"OrExpr";
  OrExpr      = sepBy(V"AndExpr", "OrOp", "OrExpr");
  AndExpr     = sepBy(V"RelExpr", "AndOp", "AndExpr");
  RelExpr     = sepBy(V"BOrExpr", "RelOp", "RelExpr");
  BOrExpr     = sepBy(V"BXorExpr", "BOrOp", "BOrExpr");
  BXorExpr    = sepBy(V"BAndExpr", "BXorOp", "BXorExpr");
  BAndExpr    = sepBy(V"ShiftExpr", "BAndOp", "BAndExpr");
  ShiftExpr   = sepBy(V"ConcatExpr", "ShiftOp", "ShiftExpr");
  ConcatExpr  = V"SumExpr"
              * (sVs"ConcatOp" * expect(V"ConcatExpr", "ConcatExpr"))^-1;
  SumExpr     = sepBy(V"ProdExpr", "SumOp", "SumExpr");
  ProdExpr    = sepBy(V"UnOrPowExpr", "ProdOp", "ProdExpr");
  UnOrPowExpr = V"UnaryExpr" + V"PowExpr";
  UnaryExpr   = Vs"UnaryOp" * expect(V"UnOrPowExpr", "UnaryExpr");
  PowExpr     = V"SimpleExpr"
              * (sVs"PowOp" * expect(V"UnOrPowExpr", "PowExpr"))^-1;

  SimpleExpr = V"Nil"
             + V"Dots"
             + V"Boolean"
             + V"Number"
             + V"String"
             + V"FuncDef"
             + V"Table"
             + V"SuffixedExpr";

  SuffixedExpr  = V"PrimaryExpr" * (V"Skip" * (V"Index" + V"Call"))^0;
  PrimaryExpr   = V"Name" + V"ParenExpr";
  ParenExpr     = Vs"ParenOpen" * expect(Vs"Expression", "ExprParen") * expect(V"ParenClose", "CParenExpr");
  Index         = V"FieldIndex" + V"ArrayIndex";
  FieldIndex    = Vs"FieldOp" * expect(V"Field", "NameIndex");
  ArrayIndex    = Vs"IndexOpen" * expect(Vs"Expression", "ExprIndex") * expect(V"IndexClose", "CBracketIndex");
  Call          = V"MethodCall" + V"FuncCall";
  FuncCall      = V"CallArgs";
  MethodCall    = Vs"MethodIndex" * expect(V"CallArgs", "MethArgs");
  MethodIndex   = Vs"MethodOp" * expect(V"Field", "NameMeth");
  CallArgs      = Vs"ParenOpen" * (commaSep(V"Expression", "ArgList") * V"Skip")^-1 * expect(V"ParenClose", "CParenArgs")
                + V"Table"
                + V"String";

  -- function () ... end
  FuncDef   = Vs"FuncCmd" * V"FuncBody";

  -- Table Constructor
  Table         = Vs"TableOpen"
                * Vs"TabEntries"^-1
                * expect(V"TableClose", "CBraceTable");
  TabEntries    = sepBy(V"TabEntryPair" + V"TabEntryValue", "TabEntrySep")
                * V"TabEntrySep"^-1;
  TabEntryPair  = Vs"TabEntryKey"
                * expect(Vs"AssignOp", "EqField")
                * expect(V"TabEntryValue", "ExprField");
  TabEntryKey   = V"TabEntryArray"
                + V"TabEntryField";
  TabEntryArray = Vs"IndexOpen"
                * expect(Vs"Expression", "ExprFKey")
                * expect(V"IndexClose", "CBracketFKey");
  TabEntryField = Vs"Field" * #(V"AssignOp");
  TabEntryValue = V"Expression";
  TabEntrySep   = V"Comma"
                + V"Semicolon";

  -- Variable Names
  Name  = V"Identifier";
  Field = V"Identifier";

  Skip = (V"Space" + V"Comment")^0;

  -- Lexer ---------------------------------------------------------------------

  -- Whitespace
  Space = space^1;

  -- Comments
  Comment = V"LongComment"
          + V"LineComment";
  LongComment = P"--" * V"DblSqBracket";
  LineComment = P"--" * (P(1) - P"\n")^0;

  -- Identifiers
  Identifier = -V"Keywords" * V"IdentStart" * V"IdentRest"^0;
  IdentStart = alpha + P"_";
  IdentRest  = alnum + P"_";

  -- Strings
  String = V"QuoteString"
         + V"LongString";

  LongString   = V"DblSqBracket";
  DblSqBracket = V"DbSqBkOpen" * V"DbSqBkData" * expect(V"DbSqBkClose", "CloseLStr");
  DbSqBkOpen   = "[" * Cg(V"DbSqBkEquals", "DbSqBkEquals") * "[" * V"DbSqBkOpLine";
  DbSqBkClose  = "]" * V"DbSqBkEquals" * "]";
  DbSqBkData   = (P(1) - V"DbSqBkAbort")^0;
  DbSqBkAbort  = Cmt("]" * C(V"DbSqBkEquals") * "]" * Cb("DbSqBkEquals"),
                     function (_, _, closeEq, openEq)
                       return #openEq == #closeEq
                     end);
  DbSqBkOpLine = P"\n"^-1;
  DbSqBkEquals = P"="^0;

  QuoteString  = V"QuoteOpen" * V"QuoteData" * expect(V"QuoteClose", "Quote");
  QuoteOpen    = Cg(S[["']], "QuoteOpen");
  QuoteClose   = S[["']];
  QuoteData    = (V"QuoteEscape" + (P(1)-V"QuoteAbort"))^0;
  QuoteAbort   = P"\n" + Cmt(C(S[["']]) * Cb("QuoteOpen"),
                             function (_, _, closeQt, openQt)
                               return openQt == closeQt
                             end);
  QuoteEscape  = V"QtEscSymbol" * ( V"QtEscChar"
                                  + V"QtEscSpace"
                                  + V"QtEscHexa"
                                  + V"QtEscUnicode"
                                  + V"CharCodeDec"
                                  + throw("InvalidQuoteEscape")
                                  );
  QtEscSymbol  = P"\\";
  QtEscChar    = S"abfnrtv\n\r\\\"\'";
  QtEscSpace   = P"z" * space^0;
  QtEscHexa    = P"x" * expect(V"CharCodeHex", "HexEsc");
  QtEscUnicode = P"u" * expect("{", "OBraceUEsc")
               * expect(V"UnicodeHex", "DigitUEsc")
               * expect("}", "CBraceUEsc");
  CharCodeDec  = digit * digit^-2;
  CharCodeHex  = xdigit * xdigit;
  UnicodeHex   = xdigit^1;

  -- Numbers
  Number = V"NumberHex"
         + V"NumberDec";

  NumberHex     = V"NumHexFloat" * V"NumHexExpo"^-1;
  NumHexExpo    = S"pP" * S"+-"^-1 * expect(V"NumDecDigits", "DigitExpo");
  NumHexFloat   = V"NumHexInteger" * (P"." * xdigit^0)^-1
                + V"NumHexPrefix" * P"." * -P"." * expect(V"NumHexDigits", "DigitDeci");
  NumHexInteger = V"NumHexPrefix" * expect(V"NumHexDigits", "DigitHex");
  NumHexDigits  = xdigit^1;
  NumHexPrefix  = P"0" * S"xX";

  NumberDec     = V"NumDecFloat" * V"NumDecExpo"^-1;
  NumDecExpo    = S"eE" * S"+-"^-1 * expect(V"NumDecDigits", "DigitExpo");
  NumDecFloat   = V"NumDecInteger" * (P"." * digit^0)^-1
                + P"." * -P"." * expect(V"NumDecDigits", "DigitDeci");
  NumDecInteger = V"NumDecDigits";
  NumDecDigits  = digit^1;

  -- Booleans
  Boolean = V"True"
          + V"False";

  -- Other Groups
  RelOp     = V"NotEqOp"
            + V"EqualOp"
            + V"LessEqOp"
            + V"GreatEqOp"
            + V"LesserOp"
            + V"GreaterOp";
  ShiftOp   = V"LShiftOp"
            + V"RShiftOp";
  SumOp     = V"AddOp"
            + V"SubOp";
  ProdOp    = V"MulOp"
            + V"IntDivOp"
            + V"DivOp"
            + V"ModOp";
  UnaryOp   = V"NotOp"
            + V"NegOp"
            + V"SizeOp"
            + V"BNotOp";
  Keywords  = V"AndOp"
            + V"BreakCmd"
            + V"DoCmd"
            + V"ElIfCmd"
            + V"ElseCmd"
            + V"EndCmd"
            + V"False"
            + V"ForCmd"
            + V"FuncCmd"
            + V"GotoCmd"
            + V"IfCmd"
            + V"InCmd"
            + V"LocalCmd"
            + V"Nil"
            + V"NotOp"
            + V"OrOp"
            + V"RepeatCmd"
            + V"ReturnCmd"
            + V"ThenCmd"
            + V"True"
            + V"UntilCmd"
            + V"WhileCmd";

  -- Values
  Nil       = kw"nil";
  False     = kw"false";
  True      = kw"true";

  -- Commands
  BreakCmd  = kw"break";
  DoCmd     = kw"do";
  ElIfCmd   = kw"elseif";
  ElseCmd   = kw"else";
  EndCmd    = kw"end";
  ForCmd    = kw"for";
  FuncCmd   = kw"function";
  GotoCmd   = kw"goto";
  IfCmd     = kw"if";
  InCmd     = kw"in";
  LocalCmd  = kw"local";
  RepeatCmd = kw"repeat";
  ReturnCmd = kw"return";
  ThenCmd   = kw"then";
  UntilCmd  = kw"until";
  WhileCmd  = kw"while";

  -- Operators
  FieldOp   = P"." * -P".";
  MethodOp  = P":" * -P":";
  AssignOp  = P"=" * -P"=";
  OrOp      = kw"or";
  AndOp     = kw"and";
  NotEqOp   = P"~=";
  EqualOp   = P"==";
  LessEqOp  = P"<=";
  GreatEqOp = P">=";
  LesserOp  = P"<";
  GreaterOp = P">";
  BOrOp     = P"|";
  BXorOp    = P"~" * -P"=";
  BAndOp    = P"&";
  LShiftOp  = P"<<";
  RShiftOp  = P">>";
  ConcatOp  = P"..";
  AddOp     = P"+";
  SubOp     = P"-";
  MulOp     = P"*";
  IntDivOp  = P"//";
  DivOp     = P"/" * -P"/";
  ModOp     = P"%";
  NotOp     = kw"not";
  NegOp     = P"-";
  SizeOp    = P"#";
  BNotOp    = P"~";
  PowOp     = P"^";

  -- Symbols
  Comma      = P",";
  Semicolon  = P";";
  Dots       = P"...";
  ParenOpen  = P"(";
  ParenClose = P")";
  IndexOpen  = P"[" * -P(S"=[");
  IndexClose = P"]";
  TableOpen  = P"{";
  TableClose = P"}";
  LabelEdge  = P"::";
}

return {
  grammar = grammar,
}