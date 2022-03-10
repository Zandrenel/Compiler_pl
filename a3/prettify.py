import sys

file_in = sys.argv[1]

file_out = sys.argv[2]


with open(file_in, 'r') as fi:
    with open(file_out, 'w') as fo:
        contents = fi.read()
        level = 0
        #when hits a "(" go to new line and increase indent
        for char in range(len(contents)):
            fo.write(contents[char])
            if contents[char] == '(':
                level += 1
                fo.write('\n')
                for i in range(level):
                    fo.write('-')
            elif contents[char] == ')':                
                level -= 1
                fo.write('\n')
                for i in range(level-1):
                    fo.write('-')
            """
            elif contents[char] == ',':
                fo.write('\n')
                for i in range(level):
                    fo.write('-')
            """

      
