local lpeg = require "lpeglabel"

local P, S, V = lpeg.P, lpeg.S, lpeg.V
local C, Carg, Cb, Cc = lpeg.C, lpeg.Carg, lpeg.Cb, lpeg.Cc
local Cf, Cg, Cmt, Cp, Cs, Ct = lpeg.Cf, lpeg.Cg, lpeg.Cmt, lpeg.Cp, lpeg.Cs, lpeg.Ct
local Lc, T = lpeg.Lc, lpeg.T

local alpha, digit, alnum = lpeg.alpha, lpeg.digit, lpeg.alnum
local xdigit = lpeg.xdigit
local space = lpeg.space

-- error message auxiliary functions

local labels = {
  { "ErrExtra", "unexpected character(s), expected EOF" },
  { "ErrInvalidStat", "unexpected token, invalid start of statement" },

  { "ErrEndIf", "expected 'end' to close the if statement" },
  { "ErrExprIf", "expected a condition after 'if'" },
  { "ErrThenIf", "expected 'then' after the condition" },
  { "ErrExprEIf", "expected a condition after 'elseif'" },
  { "ErrThenEIf", "expected 'then' after the condition" },

  { "ErrEndDo", "expected 'end' to close the do block" },
  { "ErrExprWhile", "expected a condition after 'while'" },
  { "ErrDoWhile", "expected 'do' after the condition" },
  { "ErrEndWhile", "expected 'end' to close the while loop" },
  { "ErrUntilRep", "expected 'until' at the end of the repeat loop" },
  { "ErrExprRep", "expected a conditions after 'until'" },

  { "ErrForRange", "expected a numeric or generic range after 'for'" },
  { "ErrEndFor", "expected 'end' to close the for loop" },
  { "ErrExprFor1", "expected a starting expression for the numeric range" },
  { "ErrCommaFor", "expected ',' to split the start and end of the range" },
  { "ErrExprFor2", "expected an ending expression for the numeric range" },
  { "ErrExprFor3", "expected a step expression for the numeric range after ','" },
  { "ErrInFor", "expected '=' or 'in' after the variable(s)" },
  { "ErrEListFor", "expected one or more expressions after 'in'" },
  { "ErrDoFor", "expected 'do' after the range of the for loop" },

  { "ErrDefLocal", "expected a function definition or assignment after local" },
  { "ErrNameLFunc", "expected a function name after 'function'" },
  { "ErrEListLAssign", "expected one or more expressions after '='" },
  { "ErrEListAssign", "expected one or more expressions after '='" },

  { "ErrFuncName", "expected a function name after 'function'" },
  { "ErrOParenPList", "expected '(' for the parameter list" },
  { "ErrCParenPList", "expected ')' to close the parameter list" },
  { "ErrEndFunc", "expected 'end' to close the function body" },
  { "ErrFuncParSpec", "expected a variable name or '...' after ','" },

  { "ErrLabel", "expected a label name after '::'" },
  { "ErrCloseLabel", "expected '::' after the label" },
  { "ErrGoto", "expected a label after 'goto'" },
  { "ErrRetList", "expected an expression after ',' in the return statement" },

  { "ErrVarList", "expected a variable name after ','" },
  { "ErrExprList", "expected an expression after ','" },

  { "ErrOrExpr", "expected an expression after 'or'" },
  { "ErrAndExpr", "expected an expression after 'and'" },
  { "ErrRelExpr", "expected an expression after the relational operator" },
  { "ErrBOrExpr", "expected an expression after '|'" },
  { "ErrBXorExpr", "expected an expression after '~'" },
  { "ErrBAndExpr", "expected an expression after '&'" },
  { "ErrShiftExpr", "expected an expression after the bit shift" },
  { "ErrConcatExpr", "expected an expression after '..'" },
  { "ErrAddExpr", "expected an expression after the additive operator" },
  { "ErrMulExpr", "expected an expression after the multiplicative operator" },
  { "ErrUnaryExpr", "expected an expression after the unary operator" },
  { "ErrPowExpr", "expected an expression after '^'" },

  { "ErrExprParen", "expected an expression after '('" },
  { "ErrCParenExpr", "expected ')' to close the expression" },
  { "ErrNameIndex", "expected a field name after '.'" },
  { "ErrExprIndex", "expected an expression after '['" },
  { "ErrCBracketIndex", "expected ']' to close the indexing expression" },
  { "ErrNameMeth", "expected a method name after ':'" },
  { "ErrMethArgs", "expected some arguments for the method call (or '()')" },

  { "ErrArgList", "expected an expression after ',' in the argument list" },
  { "ErrCParenArgs", "expected ')' to close the argument list" },

  { "ErrCBraceTable", "expected '}' to close the table constructor" },
  { "ErrEqField", "expected '=' after the table key" },
  { "ErrExprField", "expected an expression after '='" },
  { "ErrExprFKey", "expected an expression after '[' for the table key" },
  { "ErrCBracketFKey", "expected ']' to close the table key" },

  { "ErrDigitHex", "expected one or more hexadecimal digits after '0x'" },
  { "ErrDigitDeci", "expected one or more digits after the decimal point" },
  { "ErrDigitExpo", "expected one or more digits for the exponent" },

  { "ErrQuote", "unclosed string" },
  { "ErrHexEsc", "expected exactly two hexadecimal digits after '\\x'" },
  { "ErrOBraceUEsc", "expected '{' after '\\u'" },
  { "ErrDigitUEsc", "expected one or more hexadecimal digits for the UTF-8 code point" },
  { "ErrCBraceUEsc", "expected '}' after the code point" },
  { "ErrEscSeq", "invalid escape sequence" },
  { "ErrCloseLStr", "unclosed long string" },
}

local function throw(label)
  label = "Err" .. label
  for i, labelinfo in ipairs(labels) do
    if labelinfo[1] == label then
      return T(i)
    end
  end
  error("Label not found: " .. label)
end

local function expect (patt, label)
  return patt + throw(label)
end


-- regular combinators and auxiliary functions

local function kw (w) return P(w) * -V"IdentRest" end
local function Vs (n) return V(n) * V"Skip" end
local function sV (n) return V"Skip" * V(n) end
local function sVs (n) return sV(n) * V"Skip" end

local function sepBy (patt, sep, label)
  if label then
    return patt * Cg(V"Skip" * sep * V"Skip" * expect(patt, label))^0 -- TODO: remove this capture
  else
    return patt * Cg(V"Skip" * sep * V"Skip" * patt)^0
  end
end

local function commaSep (patt, label)
  return sepBy(patt, V"Comma", label)
end

local grammar = { V"Lua",
  Lua      = V"Shebang"^-1 * sV"Block" * expect(P(-1), "Extra");
  Shebang  = P"#!" * (P(1) - P"\n")^0;

  Block       = Vs"Stat"^0 * Vs"RetStat"^-1;
  Stat        = V"IfStat"
              + V"DoStat"
              + V"WhileStat"
              + V"RepeatStat"
              + V"ForStat"
              + V"LocalStat"
              + V"FuncStat"
              + V"BreakStat"
              + V"LabelStat"
              + V"GoToStat"
              + V"CallExpr"
              + V"Assignment"
              + V"Semicolon"
              + -V"BlockEnd" * throw("InvalidStat");
  BlockEnd    = P"return" + "end" + "elseif" + "else" + "until" + -1;

  -- if ... then ... elseif ... then ... else ... end
  IfStat     = V"IfPart"
             * V"ElseIfPart"^0
             * V"ElsePart"^-1
             * expect(V"EndCmd", "EndIf");
  IfPart     = Vs"IfCmd"
             * expect(Vs"Expression", "ExprIf")
             * expect(Vs"ThenCmd", "ThenIf")
             * V"Block";
  ElseIfPart = Vs"ElIfCmd"
             * expect(Vs"Expression", "ExprEIf")
             * expect(Vs"ThenCmd", "ThenEIf")
             * V"Block";
  ElsePart   = Vs"ElseCmd"
             * V"Block";

  -- do ... end
  DoStat = Vs"DoCmd"
         * V"Block"
         * expect(V"EndCmd", "EndDo");

  -- while ... do ... end
  WhileStat = Vs"WhileCmd"
            * expect(V"Expression", "ExprWhile")
            * sV"WhileBody";
  WhileBody = expect(Vs"DoCmd", "DoWhile")
            * V"Block"
            * expect(V"EndCmd", "EndWhile");

  -- repeat ... until
  RepeatStat = Vs"RepeatCmd"
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
  ForBody     = expect(Vs"DoCmd", "DoFor")
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
  FuncBody    = Vs"FuncParams"
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

  RetStat    = Vs"ReturnCmd"
             * commaSep(V"Expression", "RetList")^-1
             * (sV"Semicolon")^-1;

  NameList = commaSep(V"Name");
  VarList  = commaSep(V"VarExpr", "VarList");
  ExprList = commaSep(V"Expression", "ExprList");

  Expression  = V"OrExpr";
  OrExpr      = sepBy(V"AndExpr", V"OrOp", "OrExpr");
  AndExpr     = sepBy(V"RelExpr", V"AndOp", "AndExpr");
  RelExpr     = sepBy(V"BOrExpr", V"RelOp", "RelExpr");
  BOrExpr     = sepBy(V"BXorExpr", V"BOrOp", "BOrExpr");
  BXorExpr    = sepBy(V"BAndExpr", V"BXorOp", "BXorExpr");
  BAndExpr    = sepBy(V"ShiftExpr", V"BAndOp", "BAndExpr");
  ShiftExpr   = sepBy(V"ConcatExpr", V"ShiftOp", "ShiftExpr");
  ConcatExpr  = V"AddExpr"
              * (sVs"ConcatOp" * expect(V"ConcatExpr", "ConcatExpr"))^-1;
  AddExpr     = sepBy(V"MulExpr", V"SumOp", "AddExpr");
  MulExpr     = sepBy(V"UnOrPowExpr", V"ProdOp", "MulExpr");
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

  CallExpr  = Cmt(V"SuffixedExpr", function(s, i, ...) return string.sub(s, i-1, i-1) == ")", ... end);
  VarExpr   = Cmt(V"SuffixedExpr", function(s, i, ...) return string.sub(s, i-1, i-1) ~= ")", ... end);

  SuffixedExpr  = V"PrimaryExpr" * (V"Skip" * (V"Index" + V"Call"))^0;
  PrimaryExpr   = V"Name" + V"ParenExpr";
  ParenExpr     = Vs"ParenOpen" * expect(V"Expression", "ExprParen") * expect(V"ParenClose", "CParenExpr");
  Index         = V"FieldIndex" + V"ArrayIndex";
  FieldIndex    = Vs"FieldOp" * expect(V"Field", "NameIndex");
  ArrayIndex    = Vs"IndexOpen" * expect(Vs"Expression", "ExprIndex") * expect(V"IndexClose", "CBracketIndex");
  Call          = V"MethodCall" + V"FuncCall";
  FuncCall      = V"FuncArgs";
  MethodCall    = Vs"MethodIndex" * expect(V"FuncArgs", "MethArgs");
  MethodIndex   = Vs"MethodOp" * expect(V"Field", "NameMeth");

  FuncDef   = Vs"FuncCmd" * V"FuncBody";
  FuncArgs  = Vs"ParenOpen" * commaSep(V"Expression", "ArgList")^-1 * expect(V"ParenClose", "CParenArgs")
            + V"Table"
            + V"String";

  -- Tables
  Table       = Vs"TableOpen" * V"TabEntries"^-1 * expect(sV"TableClose", "CBraceTable");
  TabEntries  = sepBy(V"TabEntryPair" + V"TabEntryVal", V"TabEntrySep") * V"TabEntrySep"^-1;
  TabEntryPair = Vs"TabEntryKey" * expect(Vs"AssignOp", "EqField") * expect(V"TabEntryVal", "ExprField");
  TabEntryKey  = Vs"IndexOpen" * expect(Vs"Expression", "ExprFKey") * expect(V"IndexClose", "CBracketFKey")
               + V"Field" * #(sV"AssignOp");
  TabEntryVal  = V"Expression";
  TabEntrySep  = V"Comma" + V"Semicolon";

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
                     function (s, i, closeEq, openEq)
                       return #openEq == #closeEq
                     end);
  DbSqBkOpLine = P"\n"^-1;
  DbSqBkEquals = P"="^0;

  QuoteString  = V"QuoteOpen" * V"QuoteData" * expect(V"QuoteClose", "Quote");
  QuoteOpen    = Cg(S[["']], "QuoteOpen");
  QuoteClose   = S[["']];
  QuoteData    = (V"QuoteEscape" + (P(1)-V"QuoteAbort"))^0;
  QuoteAbort   = P"\n" + Cmt(C(S[["']]) * Cb("QuoteOpen"),
                             function (s, i, closeQt, openQt)
                               return openQt == closeQt
                             end);
  QuoteEscape  = V"QtEscSymbol" * ( V"QtEscChar"
                                  + V"QtEscSpace"
                                  + V"QtEscHexa"
                                  + V"QtEscUnicode"
                                  + V"CharCodeDec"
                                  + throw("EscSeq")
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
  errors = labels,
}