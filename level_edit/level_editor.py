import sys

level_objs = {'x' : '$3F', '.' : '$20', 'T': '$3C', '0': '$2F'}

def compress_all_levels(level_data, level_layout):
    
    level_string = ''
    while True:
        #Get and append level name.
        if (level_name := level_data.readline()) == '':
            break

        level_name = level_name.rstrip('\n')

        level_index = level_layout.index(level_name)

        level_string += level_name + ':\n'
        
        #[N, E, S, W]
        level_adj_list = [level_index-3, 
                          level_index+1,
                          level_index+3,
                          level_index-1]

        for level_neighbours in level_adj_list:
            if level_neighbours >= len(level_layout) or level_neighbours < 0:
                neighbour_loc = f'\tdc.b\t$00, $00\n'
            else:
                neighbour_loc = f'\tdc.b\t<{level_layout[level_neighbours]}, >{level_layout[level_neighbours]}\n'
            level_string += neighbour_loc

        #506 blocks + 23 \r\n or \n's == 528
        print(level_name)
        compressed_level = compress_level(level_data.read(529)) + '\n'
        level_string += compressed_level

    return level_string


def compress_level(data):
    compressed_level = []
    level_length = 506
    i = 0
    cleaned_data = data.replace('\n', '')

    while i < level_length:
        run_length = 1

        while (i + run_length < len(cleaned_data) and
            (cleaned_data[i] == cleaned_data[i + run_length] or
             cleaned_data[i + run_length] == '\n')and
            run_length < 255):
            # if cleaned_data[i + run_length] == '\n':
            #     continue
            # print("something")
            run_length += 1

        run_length_string = '${:02X}'.format(run_length)
        compressed_level.append(run_length_string)

        #I bet this will bite me in the ass later.
        #It did.
        if(i < len(cleaned_data)):  #Need this check because of some final level access issue.
            compressed_level.append(level_objs.get(cleaned_data[i], '$20'))
        i += run_length

    #TODO clean up the compressed level string.
    print(i)
    i = 0
    while(i < len(data)):
        if not (i % 21):
            compressed_level.insert(i, "\n\tdc.b\t")
        i += 1
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
        raw_level_layout_line = f_levels.readline().rstrip('\n')
        print(raw_level_layout_line)
        extracted_level_layout = extract_level_layout(raw_level_layout_line)
        compressed_levels = compress_all_levels(f_levels, extracted_level_layout)

    #a for write append.
    with open(output_file, 'w') as f_output:
        f_output.write(compressed_levels)

if __name__== "__main__":
    main()