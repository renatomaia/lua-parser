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

local function kw (str)
  return P(str) * -V"IdentRest"
end

local function tk (name)
  return V(name) * V"Skip"
end

local function sepBy (patt, sep, label)
  if label then
    return patt * Cg(sep * expect(patt, label))^0 -- TODO: remove this capture
  else
    return patt * Cg(sep * patt)^0
  end
end

local function commaSep (patt, label)
  return sepBy(patt, tk"Comma", label)
end

local grammar = { V"Lua",
  Lua      = V"Shebang"^-1 * V"Skip" * V"Block" * expect(P(-1), "Extra");
  Shebang  = P"#!" * (P(1) - P"\n")^0;

  Block       = V"Stat"^0 * V"RetStat"^-1;
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
              + tk"Semicolon"
              + -V"BlockEnd" * throw("InvalidStat");
  BlockEnd    = P"return" + "end" + "elseif" + "else" + "until" + -1;

  -- if ... then ... elseif ... then ... else ... end
  IfStat     = V"IfPart"
             * V"ElseIfPart"^0
             * V"ElsePart"^-1
             * expect(tk"EndCmd", "EndIf");
  IfPart     = tk"IfCmd"
             * expect(V"Expression", "ExprIf")
             * expect(tk"ThenCmd", "ThenIf")
             * V"Block";
  ElseIfPart = tk"ElIfCmd"
             * expect(V"Expression", "ExprEIf")
             * expect(tk"ThenCmd", "ThenEIf")
             * V"Block";
  ElsePart   = tk"ElseCmd"
             * V"Block";

  -- do ... end
  DoStat = tk"DoCmd"
         * V"Block"
         * expect(tk"EndCmd", "EndDo");

  -- while ... do ... end
  WhileStat = tk"WhileCmd" * expect(V"Expression", "ExprWhile")
            * V"WhileBody";
  WhileBody = expect(tk"DoCmd", "DoWhile")
            * V"Block"
            * expect(tk"EndCmd", "EndWhile");

  -- repeat ... until
  RepeatStat = tk"RepeatCmd"
             * V"Block"
             * expect(tk"UntilCmd", "UntilRep") * expect(V"Expression", "ExprRep");

  -- for ... do ... end
  ForStat     = tk"ForCmd" * expect(V"ForNumSpec" + V"ForInSpec", "ForRange")
              * V"ForBody";
  ForInSpec   = V"NameList"
              * expect(tk"InCmd", "InFor")
              * expect(V"ExprList", "EListFor");
  ForNumSpec  = tk"Name"
              * tk"AssignOp"
              * V"ForNumRange";
  ForNumRange = expect(V"Expression", "ExprFor1")
              * expect(tk"Comma", "CommaFor")
              * expect(V"Expression", "ExprFor2")
              * (tk"Comma" * expect(V"Expression", "ExprFor3"))^-1;
  ForBody     = expect(tk"DoCmd", "DoFor")
              * V"Block"
              * expect(tk"EndCmd", "EndFor");

  -- local ...
  LocalStat = tk"LocalCmd" * expect(V"LocalFunc" + V"LocalVar", "DefLocal");
  LocalFunc = tk"FuncCmd" * expect(tk"Name", "NameLFunc") * V"FuncBody";
  LocalVar  = V"NameList" * ( tk"AssignOp" * expect(V"ExprList", "EListLAssign")
                            + P(true) -- or empty
                            );

  -- ... = ...
  Assignment  = V"VarList" * tk"AssignOp" * expect(V"ExprList", "EListAssign");

  -- function ... (...) ... end
  FuncStat    = tk"FuncCmd" * expect(V"FuncName", "FuncName") * V"FuncBody";
  FuncName    = tk"Name"
              * (V"FieldIndex")^0
              * (V"MethodIndex")^-1;
  FuncBody    = V"FuncParams"
              * V"Block"
              * expect(tk"EndCmd", "EndFunc");
  FuncParams  = expect(tk"ParenOpen", "OParenPList")
              * V"FuncParSpec"
              * expect(tk"ParenClose", "CParenPList");
  FuncParSpec = V"FuncParName" + tk"Dots" + P(true);
  FuncParName = V"NameList" * (tk"Comma" * expect(tk"Dots", "FuncParSpec"))^-1;

  LabelStat  = tk"LabelEdge"
             * expect(tk"Identifier", "Label")
             * expect(tk"LabelEdge", "CloseLabel");
  GoToStat   = tk"GotoCmd" * expect(tk"Identifier", "Goto");
  BreakStat  = tk"BreakCmd";
  RetStat    = tk"ReturnCmd" * commaSep(V"Expression", "RetList")^-1 * tk"Semicolon"^-1;

  NameList  = commaSep(tk"Name");
  VarList   = commaSep(V"VarExpr", "VarList");
  ExprList  = commaSep(V"Expression", "ExprList");

  Expression  = V"OrExpr";
  OrExpr      = sepBy(V"AndExpr", tk"OrOp", "OrExpr");
  AndExpr     = sepBy(V"RelExpr", tk"AndOp", "AndExpr");
  RelExpr     = sepBy(V"BOrExpr", tk"RelOp", "RelExpr");
  BOrExpr     = sepBy(V"BXorExpr", tk"BOrOp", "BOrExpr");
  BXorExpr    = sepBy(V"BAndExpr", tk"BXorOp", "BXorExpr");
  BAndExpr    = sepBy(V"ShiftExpr", tk"BAndOp", "BAndExpr");
  ShiftExpr   = sepBy(V"ConcatExpr", tk"ShiftOp", "ShiftExpr");
  ConcatExpr  = V"AddExpr" * (tk"ConcatOp" * expect(V"ConcatExpr", "ConcatExpr"))^-1;
  AddExpr     = sepBy(V"MulExpr", tk"SumOp", "AddExpr");
  MulExpr     = sepBy(V"UnOrPowExpr", tk"ProdOp", "MulExpr");
  UnOrPowExpr = V"UnaryExpr" + V"PowExpr";
  UnaryExpr   = tk"UnaryOp" * expect(V"UnOrPowExpr", "UnaryExpr");
  PowExpr     = V"SimpleExpr" * (tk"PowOp" * expect(V"UnOrPowExpr", "PowExpr"))^-1;

  SimpleExpr = tk"Nil"
             + tk"Boolean"
             + tk"Number"
             + tk"String"
             + tk"Dots"
             + V"FuncDef"
             + V"Table"
             + V"SuffixedExpr";

  --CallExpr  = Cmt(V"SuffixedExpr", function(s, i, exp, ...) return exp.tag == "Call" or exp.tag == "Invoke", exp end);
  --VarExpr   = Cmt(V"SuffixedExpr", function(s, i, exp, ...) return exp.tag == "Name" or exp.tag == "Index", exp end);

  SuffixedExpr  = V"PrimaryExpr" * (V"Index" + V"CallOp")^0;
  PrimaryExpr   = tk"Name" + V"ParenExpr";
  ParenExpr     = tk"ParenOpen" * expect(V"Expression", "ExprParen") * expect(tk"ParenClose", "CParenExpr");
  Index         = V"FieldIndex" + V"ArrayIndex";
  FieldIndex    = tk"FieldOp" * expect(tk"Field", "NameIndex");
  ArrayIndex    = tk"IndexOpen" * expect(V"Expression", "ExprIndex") * expect(tk"IndexClose", "CBracketIndex");
  CallOp        = V"Invoke" + V"Call";
  Invoke        = V"MethodIndex" * expect(V"FuncArgs", "MethArgs");
  Call          = V"FuncArgs";
  MethodIndex   = tk"MethodOp" * expect(tk"Field", "NameMeth");

  FuncDef   = tk"FuncCmd" * V"FuncBody";
  FuncArgs  = tk"ParenOpen" * commaSep(V"Expression", "ArgList")^-1 * expect(tk"ParenClose", "CParenArgs")
            + V"Table"
            + tk"String";

  Table       = tk"TableOpen" * V"TabEntries"^-1 * expect(tk"TableClose", "CBraceTable");
  TabEntries  = sepBy(V"TabEntryPair" + V"TabEntryVal", V"TabEntrySep") * V"TabEntrySep"^-1;
  TabEntryPair = V"TabEntryKey" * expect(tk"AssignOp", "EqField") * expect(V"TabEntryVal", "ExprField");
  TabEntryKey  = tk"IndexOpen" * expect(V"Expression", "ExprFKey") * expect(tk"IndexClose", "CBracketFKey")
               + tk"Field" * #V"AssignOp";
  TabEntryVal  = V"Expression";
  TabEntrySep  = tk"Comma" + tk"Semicolon";

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