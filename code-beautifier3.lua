local lpeg = require "lpeglabel"

lpeg.locale(lpeg)

local P, S, V = lpeg.P, lpeg.S, lpeg.V
local C, Carg, Cb, Cc = lpeg.C, lpeg.Carg, lpeg.Cb, lpeg.Cc
local Cf, Cg, Cmt, Cp, Cs, Ct = lpeg.Cf, lpeg.Cg, lpeg.Cmt, lpeg.Cp, lpeg.Cs, lpeg.Ct
local Lc, T = lpeg.Lc, lpeg.T

local alpha, digit, alnum = lpeg.alpha, lpeg.digit, lpeg.alnum
local xdigit = lpeg.xdigit

local lua53 = require "lua-parser.grammar"

local errors = require "lua-parser.errors"
local throw = errors.throw

-- regular combinators and auxiliary functions

local function tagC (tag, patt)
  return Ct(Cg(Cp(), "pos") * Cg(Cc(tag), "tag") * patt)
end

local function fill(src, dst)
  for key, value in pairs(src) do
    if dst[key] == nil then
      dst[key] = value
    end
  end
  return dst
end

local tags = {}

local function printItem(item)
  if type(item) == "string" then return io.write(item) end
  return assert(tags[item.tag], item.tag)(item)
end

local function printList(list)
  for _, item in ipairs(list) do
    printItem(item)
  end
end

local function newSeqPrint(op)
  return function (item)
    printItem(item[1])
    for i = 2, #item, 3 do
      tags.Skip(item[i+0])
      io.write(op)
      tags.Skip(item[i+1])
      printItem(item[i+2])
    end
  end
end

tags.Skip = printList
tags.Block = printList

function tags.DoStat(item)
  io.write("do")
  tags.Block(item[1])
  io.write("end")
end

function tags.IfStat(item)
  io.write("if")
  printList(item)
  io.write("end")
end

function tags.IfPart(item)
  tags.Skip(item[1])
  tags.Expression(item[2])
  tags.Skip(item[3])
  io.write("then")
  tags.Block(item[4])
end

function tags.ElseIfPart(item)
  io.write("elseif")
  tags.IfPart(item)
end

function tags.ElsePart(item)
  io.write("else")
  tags.Block(item[1])
end

function tags.WhileStat(item)
  io.write("while")
  tags.Skip(item[1])
  tags.Expression(item[2])
  tags.Skip(item[3])
  io.write("do")
  tags.Block(item[4])
  io.write("end")
end

function tags.RepeatStat(item)
  io.write("repeat")
  tags.Block(item[1])
  io.write("until")
  tags.Skip(item[2])
  tags.Expression(item[3])
end

function tags.ForStat(item)
  io.write("for")
  printList(item)
  io.write("end")
end

function tags.ForInSpec(item)
  tags.NameList(item[1])
  tags.Skip(item[2])
  io.write("in")
  tags.Skip(item[3])
  tags.ExprList(item[4])
end

tags.ForNumSpec = printList

function tags.ForNumRange(item)
  io.write("=")
  tags.Skip(item[1])
  tags.Expression(item[2])
  tags.Skip(item[3])
  io.write(",")
  tags.Skip(item[4])
  tags.Expression(item[5])
end

function tags.ForNumStep(item)
  io.write(",")
  tags.Skip(item[1])
  tags.Expression(item[2])
end

function tags.LocalStat(item)
  io.write("local")
  printList(item)
end

function tags.LocalFunc(item)
  io.write("function")
  printList(item)
  io.write("end")
end

function tags.FuncParams(item)
  io.write("(")
  for i = 1, #item, 3 do
    if i > 1 then io.write(",") end
    tags.Skip(item[i+0])
    printItem(item[i+1])
    tags.Skip(item[i+2])
  end
  io.write(")")
end

function tags.LocalVar(item)
  tags.NameList(item[1])
  if (#item > 1) then
    tags.Skip(item[2])
    io.write("=")
    tags.Skip(item[3])
    tags.ExprList(item[4])
  end
end

tags.FuncStat = tags.LocalFunc
tags.FuncName = printList

function tags.Assignment(item)
  tags.NameList(item[1])
  tags.Skip(item[2])
  io.write("=")
  tags.Skip(item[3])
  tags.VarList(item[4])
end

function tags.LabelStat(item)
  io.write("::")
  tags.Skip(item[1])
  io.write(item[2])
  tags.Skip(item[3])
  io.write("::")
end

function tags.GoToStat(item)
  io.write("goto")
  tags.Skip(item[1])
  io.write(item[2])
end

function tags.RetStat(item)
  io.write("return")
  local count = #item
  if count > 0 then
    if item[#item] == ";" then
      count = count-2
    end
    tags.Skip(item[1])
    printItem(item[2])
    for i = 3, count, 3 do
      tags.Skip(item[i+0])
      io.write(",")
      tags.Skip(item[i+1])
      printItem(item[i+2])
    end
    for i = count+1, #item do
      printItem(item[i])
    end
  end
end

tags.NameList = newSeqPrint(",")
tags.VarList  = newSeqPrint(",")
tags.ExprList = newSeqPrint(",")

tags.Expression = printList

tags.OrExpr      = newSeqPrint("or")
tags.AndExpr     = newSeqPrint("and")
tags.BOrExpr     = newSeqPrint("|")
tags.BXorExpr    = newSeqPrint("~")
tags.BAndExpr    = newSeqPrint("&")
tags.ConcatExpr  = newSeqPrint("..")
tags.PowExpr     = newSeqPrint("^")

tags.RelExpr     = printList
tags.ShiftExpr   = printList
tags.SumExpr     = printList
tags.ProdExpr    = printList
tags.UnaryExpr   = printList

tags.SuffixedExpr = printList

function tags.ParenExpr(item)
  io.write("(")
  tags.Skip(item[1])
  tags.Expression(item[2])
  tags.Skip(item[3])
  io.write(")")
end

function tags.ArrayIndex(item)
  io.write("[")
  tags.Skip(item[1])
  tags.Expression(item[2])
  tags.Skip(item[3])
  io.write("]")
end

function tags.FieldIndex(item)
  io.write(".")
  tags.Skip(item[1])
  io.write(item[2])
end

function tags.MethodIndex(item)
  io.write(":")
  tags.Skip(item[1])
  io.write(item[2])
end

function tags.CallArgs(item)
  printItem(item[1])
end

function tags.ArgList(item)
  io.write("(")
  printList(item)
  io.write(")")
end

tags.FuncDef = tags.LocalFunc

function tags.Table(item)
  io.write("{")
  printList(item)
  io.write("}")
end

function tags.TabEntryPair(item)
  printItem(item[1])
  tags.Skip(item[2])
  io.write("=")
  tags.Skip(item[3])
  printItem(item[4])
end

function tags.TabEntryArray(item)
  io.write("[")
  tags.Skip(item[1])
  tags.Expression(item[2])
  tags.Skip(item[3])
  io.write("]")
end

function tags.TabEntrySep(item)
  io.write(item[1] or ",")
end

function tags.Comment(item)
  io.write("--")
  local longlevel = item.DbSqBkEquals
  if longlevel ~= nil then
    io.write("[",longlevel,"[")
  end
  io.write(item[1])
  if longlevel ~= nil then
    io.write("]",longlevel,"]")
  end
end

function tags.String(item)
  local longlevel = item.DbSqBkEquals
  if longlevel ~= nil then
    io.write("[",longlevel,"[")
    io.write(item[1])
    io.write("]",longlevel,"]")
  else
    io.write(item.QuoteOpen)
    io.write(item[1])
    io.write(item.QuoteOpen)
  end
end

local grammar = lua53.grammar
local G = { V"Lua",
  FuncParName = grammar.NameList
              * ( V"Skip"
                * V"Comma"
                * V"Skip"
                * (V"Dots" + throw"FuncParSpec")
                )^-1,

  BreakStat = C(grammar.BreakStat),
  RelOp     = C(grammar.RelOp),
  ShiftOp   = C(grammar.ShiftOp),
  SumOp     = C(grammar.SumOp),
  ProdOp    = C(grammar.ProdOp),
  UnaryOp   = C(grammar.UnaryOp),

  -- Lexer ---------------------------------------------------------------------

  -- Whitespace
  Space = C(grammar.Space);

  -- Comments
  LineComment = P"--" * C((P(1) - P"\n")^0);

  -- Identifiers
  Identifier = C(grammar.Identifier);

  -- Strings
  DbSqBkData = C(grammar.DbSqBkData);
  QuoteData  = C(grammar.QuoteData);

  -- Numbers
  Number   = C(grammar.Number);

  -- Special Values
  Nil   = C(grammar.Nil);
  False = C(grammar.False);
  True  = C(grammar.True);
  Dots  = C(grammar.Dots);

  Semicolon = C(grammar.Semicolon);
}

for name in pairs(tags) do
  G[name] = tagC(name, assert(grammar[name], name))
end
G = fill(grammar, G)

local errors = require("lua-parser.errors")
local geterrmsg = errors.getmsgbyidx

local validator = require("lua-parser.validator")
local syntaxerror = validator.syntaxerror

local function parse (subject, filename)
  local errorinfo = { subject = subject, filename = filename }
  lpeg.setmaxstack(1000)
  local ast, erridx, sfail = lpeg.match(G, subject, nil, errorinfo)
  if not ast then
    local errpos = #subject-#sfail+1
    local errmsg = geterrmsg(erridx)
    return ast, syntaxerror(errorinfo, errpos, errmsg)
  end
  return ast
end

local file = assert(io.open(..., "r"))
local contents = assert(file:read("a"))
file:close()

local item = assert(parse(contents, ...))

do return printItem(item) end

local Viewer = require "loop.debug.Viewer"
local viewer = Viewer{
  nolabels = true,
  noindices = true,
}

viewer:write(item)
print()