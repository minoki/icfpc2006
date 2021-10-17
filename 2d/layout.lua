local t = {}
local max_width = 0
local max_height = 0
local total_width = 0
local total_height = 0
local p = coroutine.wrap(function(l)
    while true do
      local box = {}
      if l:sub(1,1) == "," then
        local width = #l
        repeat
          table.insert(box,l)
          l = coroutine.yield()
        until l:sub(1,1) == ","
        table.insert(box,l)
        table.insert(t,box)
        print(width, #box)
        max_width = math.max(max_width, width)
        max_height = math.max(max_height, #box)
        total_width = total_width + width
        total_height = total_height + #box
        l = coroutine.yield()
      else
        print("Invalid input")
      end
    end
end)
for l in io.lines() do
  p(l)
end
print(max_width, max_height)
print(total_width, total_height)
table.sort(t, function(x,y)
             local wx,hx = #x[1],#x
             local wy,hy = #y[1],#y
             if hx == hy then
               return wx > wy
             else
               return hx > hy
             end
end)
print(#t)
local result = {}
for i=1,max_height do
  result[i] = {}
end
for _,box in ipairs(t) do
  local width = #box[1]
  for i=1,#box do
    table.insert(result[i],box[i])
  end
  for i=#box+1,max_height do
    table.insert(result[i],string.rep(" ",width))
  end
end
local f = assert(io.open("out.2d","w"))
for _,line in ipairs(result) do
  f:write(table.concat(line),"\n")
end
f:close()
