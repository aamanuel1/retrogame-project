import sys

level_objs = {'x' : 63, '.' : 32, 'T': 60, '0': 47}

def compress_level_data(data):
    compressed = bytearray()
    i = 0
    while i < len(data):
        run_length = 1
        while (i + run_length < len(data) and
            data[i] == data[i + run_length] and
            run_length < 255):
            run_length += 1

        compressed.append(run_length)
        compressed.append(data[i])
        i += run_length
    return compressed

def extract_level_layout(level_layout_line):
    pass

def main():
    if len(sys.argv) != 2:
        print("Usage: python level_editor.py <level_file> <output_file>")

        level_file = sys.argv[1]
        output_file = sys.argv[2]

    extracted_level_layout = None
    raw_level_data = None

    #first line of level editor file is level layout array
    with open(level_file, 'r', encoding="utf-8") as f_levels:
        raw_level_layout_line = f.readline()
        extracted_level_layout = extract_level_layout(raw_level_layout_line)
        raw_level_data = f.read()
        compressed_levels = compress_level_data(raw_level_data)

    #wa for write append.
    with open(output_file, 'w') as f_output:
        f_output.write(extracted_level_layout)
        f_output.write(compressed_levels)

if __name__== "__main__":
    main()

