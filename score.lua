local f = io.open("publications.txt")
local t = {}
for line in f:lines() do
  local m = line:match("(%a+%.%w+=%d+@%d+%|%x+)")
  if m then
    print(m)
    table.insert(t, m)
  end
end
f:close()
local p = io.popen("./run umix.um", "w")
p:write("ftd\n")
p:write("falderal90\n")
p:write("icfp.exe\n")
for i,v in ipairs(t) do
  p:write(v .. "\n")
end
p:write("\n")
p:write("logout\n")
p:close()
