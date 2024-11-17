import sys

def rle_compress(data):
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

def main():
    if len(sys.argv) != 3:
        print("Usage: python compress_rle_gpt.py <input_file> <output_file>")

    input_file = sys.argv[1]
    output_file = sys.argv[2]

    with open(input_file, 'rb') as f:
        data = f.read()

    compressed_data = rle_compress(data)

    f.close()

    with open(output_file, 'wb') as f:
        f.write(compressed_data)

    f.close()


if name == "__main__":
    main()