local ll = {}
local max_l = 0
for l in io.lines() do
  local a, b = l:match(": ([%x ]+)\t(.+)$")
  if a ~= nil then
    local t = {}
    for x in a:gmatch("%x%x") do
      table.insert(t, "0x" .. x .. ", ")
    end
    local s = table.concat(t)
    max_l = math.max(max_l, #s)
    table.insert(ll, {s, b})
  end
end
for _, v in ipairs(ll) do
  local ws = string.rep(" ", max_l - #v[1])
  io.write(string.format("%s%s// %s\n", v[1], ws, (v[2]:gsub("\t", " "))))
end
