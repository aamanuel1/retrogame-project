import sys

level_objs = {'x' : '$3F', '.' : '$20', 'T': '$3C', '0': '$2F'}

def compress_all_levels(level_data, level_layout):
    
    level_string = ''
    while True:
        #Get and append level name.
        if (level_name := level_data.readline()) == '':
            break

        level_index = level_layout.index(level_name)

        level_string += level_name + '\n'
        
        #[N, E, S, W]
        level_adj_list = [level_index+3, 
                          level_index+1,
                          level_index-3,
                          level_index-1]

        for level_neighbours in level_adj_list:
            if level_neighbours >= len(level_layout) or < 0:
                neighbour_loc = f'\tdc.b\t$00, $00\n'
            else:
                neighbour_loc = f'\tdc.b\t<{level_neighbours}, >{level_neighbours}\n'
            level_string += neighbour_loc

        #506 blocks + 22 \r\n or \n's == 528
        compressed_level = compress_level(f.read(528)) + '\n'

    return level_string


def compress_level(data):
    compressed_level = []
    level_length = 506
    i = 0

    while i < level_length:
        run_length = 1

        while (i + run_length < level_length and
            data[i] == data[i + run_length] and
            run_length < 255):

            #skip LF or CRLF
            if data[i] == '\n':
                continue
            
            run_length += 1

        compressed_level.append(run_length)

        #I bet this will bite me in the ass later.
        compressed_level.append(get(data[i], default='$20'))
        i += run_length
    
    finished_compressed_level = ",".join(compressed_level)
    finished_compressed_level = "\tdc.b\t" + finished_compressed_level
    return finished_compressed_level

def extract_level_layout(level_layout_line):
    return level_layout_line.split(',')

def main():
    if len(sys.argv) != 2:
        print("Usage: python level_editor.py <level_file> <output_file>")

        level_file = sys.argv[1]
        output_file = sys.argv[2]

    extracted_level_layout = None
    raw_level_data = None
    compressed_levels = ''

    #first line of level editor file is level layout array
    with open(level_file, 'r', encoding="utf-8") as f_levels:
        raw_level_layout_line = f.readline()
        extracted_level_layout = extract_level_layout(raw_level_layout_line)
        compressed_levels = compress_all_levels(f_levels, extracted_level_layout)

    #a for write append.
    with open(output_file, 'a+') as f_output:
        f_output.write(compressed_levels)

if __name__== "__main__":
    main()

