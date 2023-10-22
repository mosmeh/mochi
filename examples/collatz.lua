local function collatz(n)
    return coroutine.wrap(function()
        coroutine.yield(n)
        repeat
            if n % 2 == 0 then
                n = n // 2
            else
                n = 3 * n + 1
            end
            coroutine.yield(n)
        until n == 1
    end)
end

local n = tonumber(arg[1] or 27)
for x in collatz(n) do
    print(x)
end
