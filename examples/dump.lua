local function indent(depth)
    io.write(string.rep('    ', depth))
end

local function writeValue(value)
    local t = type(value)
    if t == 'string' then
        io.write('\x1b[33m"', value, '"\x1b[m')
    elseif t == 'nil' or t == 'number' or t == 'boolean' then
        io.write('\x1b[34m', tostring(value), '\x1b[m')
    else
        io.write('\x1b[32m', tostring(value), '\x1b[m')
    end
end

local function dump(t, depth)
    for key, value in pairs(t) do
        indent(depth)
        writeValue(key)
        io.write(' = ')
        if depth > 2 or type(value) ~= 'table' or key == '_G' then
            writeValue(value)
            print(',')
        else
            print('{')
            dump(value, depth + 1)
            indent(depth)
            print('},')
        end
    end
end

print('_ENV = {')
dump(_ENV, 1)
print('}')
