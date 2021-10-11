local f = io.open("publications.txt")
local t = {}
local highests = {}
for line in f:lines() do
  local m,name,category,score = line:match("(((%a+)%.%w+)=(%d+)@%d+%|%x+)")
  if m then
    print(m)
    table.insert(t, {m, name=name, category=category, score=score})
    local c = highests[category]
    if not c then
      c = {}
      highests[category] = c
    end
    if c[name] then
      c[name] = math.max(c[name], tonumber(score))
    else
      c[name] = tonumber(score)
    end
  end
end
f:close()
local categories = {}
for k,v in pairs(highests) do
  local s = 0
  for name,t in pairs(v) do
    s = s + t
  end
  categories[k] = s
end
local categories_ = {"INTRO","CIRCS","BLNCE","BLACK","BASIC","ANTWO","ADVTR","ADVIS"}
local total = 0
for k,v in ipairs(categories_) do
  local s = categories[v] or 0
  print(v, s)
  total = total + s
end
print("Total",total)
--[[
local p = io.popen("./run umix.um", "w")
p:write("ftd\n")
p:write("falderal90\n")
p:write("icfp.exe\n")
for i,v in ipairs(t) do
  p:write(v[1] .. "\n")
end
p:write("\n")
p:write("logout\n")
p:close()
]]
