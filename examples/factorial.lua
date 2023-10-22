local function factorial(n)
    if n == 0 then
        return 1
    else
        return n * factorial(n - 1)
    end
end

local n = tonumber(arg[1] or 10)
print(n .. "! = " .. factorial(n))
