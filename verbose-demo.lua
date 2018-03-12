#!/usr/bin/env lua

--@loop.debug.Verbose{ module = myapp.verbose, flag = count }

local function calcInterest(ammount, rate, time)
  local r = ammount -->> 'r" initialized (U$$$r)
  for i=1, time do
    r = r+r*rate -->> iteration $i (U$$$r)
  end
  return r
  -->> resulting increase: U$$$(r-ammount)
end

-->> start counting...
local res = calcInterest(10, .1, 12) -->> ...done (result=U$$$res)

print("res", res)
