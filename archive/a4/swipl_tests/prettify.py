import sys

file_in = sys.argv[1]

file_out = sys.argv[2]


with open(file_in, 'r') as fi:
    with open(file_out, 'w') as fo:
        contents = fi.read()
        level = 0
        for char in range(len(contents)):
            if contents[char] == '[':
                level += 1
            elif contents[char] == ']':                
                level -= 1
            elif contents[char:char+4] == 'node':
                fo.write('\n')
                for i in range(level):
                    fo.write('  ')
            elif contents[char:char+8] == 'terminal':
                fo.write('\n')
                for i in range(level):
                    fo.write('  ')
            fo.write(contents[char])

      
