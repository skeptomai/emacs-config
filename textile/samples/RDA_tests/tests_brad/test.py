import re
import os
import textile

f = open('testcases.txt')
text = f.read()
f.close()

tests = re.split('-----', text)
for test in tests:
    test = test.split('==>', 1)
    input = test[0]
    input = input.split('\n')
    input2 = []
    for line in input:
        if not line.startswith('//'): input2.append(line)

    input = '\n'.join(input2).strip()
    output = test[1].strip().replace('\n', '').strip()
    processed = textile.textile(input).replace('\n', '').strip()

    if output != processed:

        os.system('clear')
        print
        print input
        print 
        print processed
        print
        print output
        print 

        raw_input('Press Enter')
