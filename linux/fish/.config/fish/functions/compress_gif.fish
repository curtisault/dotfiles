function compress_gif --description 'Compress a GIF using ffmpeg with various options'
    # Examples:

    # Custom framerate
    # > compress_gif -f 8 input.gif

    # Reduce colors and framerate
    # > compress_gif -f 10 -c 128 input.gif

    # Scale down to 50% size
    # > compress_gif -f 10 -s 0.5 input.gif

    # Specify output filename
    # > compress_gif -f 8 -o my_compressed.gif input.gif

    # Combine multiple options
    # > compress_gif -f 8 -s 0.5 -c 64 input.gif


    # Parse arguments
    argparse 'f/fps=' 'o/output=' 's/scale=' 'c/colors=' 'h/help' -- $argv
    or return

    # Show help
    if set -q _flag_help
        echo "Usage: compress_gif [OPTIONS] INPUT_FILE"
        echo ""
        echo "Options:"
        echo "  -f, --fps RATE       Set frame rate (default: 15)"
        echo "  -o, --output FILE    Output filename (default: INPUT_compressed.gif)"
        echo "  -s, --scale FACTOR   Scale factor (e.g., 0.5 for half size)"
        echo "  -c, --colors NUM     Max colors in palette (default: 256, try 128, 64)"
        echo "  -h, --help           Show this help"
        echo ""
        echo "Examples:"
        echo "  compress_gif input.gif"
        echo "  compress_gif -f 8 -c 128 input.gif"
        echo "  compress_gif -f 15 -s 0.5 -o small.gif input.gif"
        return 0
    end

    # Check if input file provided
    if test (count $argv) -eq 0
        echo "Error: No input file specified"
        echo "Use 'compress_gif --help' for usage information"
        return 1
    end

    set input_file $argv[1]

    # Check if input file exists
    if not test -f $input_file
        echo "Error: Input file '$input_file' not found"
        return 1
    end

    # Set defaults
    set fps $_flag_fps
    if test -z "$fps"
        set fps 15
    end

    set colors $_flag_colors
    if test -z "$colors"
        set colors 256
    end

    # Generate output filename
    set output $_flag_output
    if test -z "$output"
        set basename (basename $input_file .gif)
        set output (dirname $input_file)/$basename"_compressed.gif"
    end

    # Build ffmpeg filter
    set filter "fps=$fps"

    if set -q _flag_scale
        set filter "$filter,scale=iw*$_flag_scale:ih*$_flag_scale:flags=lanczos"
    end

    set filter "$filter,split[s0][s1];[s0]palettegen=max_colors=$colors[p];[s1][p]paletteuse"

    echo "Compressing $input_file..."
    echo "Settings: fps=$fps, colors=$colors"
    if set -q _flag_scale
        echo "Scale: $_flag_scale"
    end
    echo "Output: $output"
    echo ""

    ffmpeg -i $input_file -vf $filter $output

    if test $status -eq 0
        echo ""
        echo "Compression complete!"
        echo "Original: "(ls -lh $input_file | awk '{print $5}')
        echo "Compressed: "(ls -lh $output | awk '{print $5}')
    else
        echo "Error: Compression failed"
        return 1
    end
end
