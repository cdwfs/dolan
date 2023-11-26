-- debug
function vardump(value,depth,key)
 local line_prefix=""
 local spaces=""
 if key~=nil then
  line_prefix="["..key.."] = "
 end
 if depth==nil then
  depth=0
 else
  depth+=1
  for i=1,depth do
   spaces=spaces.." "
  end
 end
 local t=type(value)
 if t=="table" then
  local mtable=getmetatable(value)
  if mtable then
   printh(spaces.."(metatable) ")
   value=mtable
  else
   printh(spaces..line_prefix.."(table) ")
  end
  for k,v in pairs(value) do
   vardump(v,depth,k)
  end
 elseif t=="function" then
  printh(spaces..line_prefix..
         "(function)")
 elseif t=="thread"
     or t=="userdata"
     or value==nil then
  printh(spaces..tostr(value))
 else
  printh(spaces..line_prefix..
         "("..t..") "..
         tostr(value))
 end
end

-- print a range of memory to
-- the console,one byte at a
-- time.
function pb(addr,n)
 n = n or 1
 for i=0,n-1 do
  printh(tostr(addr+i,1)..": 0x"..
         sub(tostr(peek(addr+i),1),5,6))
 end
end

-- overwrite the grid with a
-- specific pattern, to simplify
-- testing
function debug_grid(grid)
 local g,r,p,y=sid_gems[1],
               sid_gems[2],
               sid_gems[3],
               sid_gems[4]
 local a,b,c,d,e,_=sid_rock2a,
                   sid_rock2b,
                   sid_rock2c,
                   sid_rock2d,
                   sid_rock1,
                   sid_empty
 return {
 --[[ blank slate
  {_,_,_,_,_,_,_,_,_,_},
  {_,_,_,_,_,_,_,_,_,_},
  {_,_,_,_,_,_,_,_,_,_},
  {_,_,_,_,_,_,_,_,_,_},
  {_,_,_,_,_,_,_,_,_,_},
  {_,_,_,_,_,_,_,_,_,_},
  {_,_,_,_,_,_,_,_,_,_},
  {_,_,_,_,_,_,_,_,_,_},
 --]]
  -- valid bury state
  {_,_,_,_,_,_,_,_,_,_},
  {g,_,_,_,_,_,_,_,_,r},
  {g,_,_,_,_,_,_,_,_,r},
  {p,_,_,_,_,_,_,_,_,g},
  {p,y,_,_,_,_,_,_,g,g},
  {g,y,g,_,r,_,_,_,y,y},
  {g,r,g,r,y,p,r,e,a,b},
  {g,r,y,r,y,p,r,g,c,d},
 }
end