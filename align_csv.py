import csv
import sys
import argparse # Import the argparse library

def align_csv(input_filepath, output_filepath):
    """
    Reads an unaligned CSV file and writes an aligned version with right-aligned columns.
    Assumes input file exists and is readable.

    Args:
        input_filepath (str): The path to the input CSV file.
        output_filepath (str): The path where the aligned CSV file will be saved.
    """
    # Read all lines using csv.reader
    with open(input_filepath, 'r', newline='') as infile:
        reader = csv.reader(infile)
        # Handle potential empty rows or rows with only whitespace
        data = [[field.strip() for field in row] for row in reader]

    # Calculate the maximum width for each column
    # Use max() with a default value of 0 in case data is empty after filtering
    num_columns = max((len(row) for row in data), default=0)
    col_widths = [0] * num_columns
    for row in data:
        for i, field in enumerate(row):
            col_widths[i] = max(col_widths[i], len(field))

    # Write the aligned data to the output file
    with open(output_filepath, 'w', newline='') as outfile:
        for row in data:
            formatted_row = []
            for i in range(num_columns): # Iterate up to the max number of columns found
                field = row[i] if i < len(row) else '' # Get field or empty string if row is short
                # Use rjust to RIGHT-align text within the calculated width
                formatted_row.append(field.rjust(col_widths[i]))

            # Join with a comma and a space
            outfile.write(', '.join(formatted_row) + '\n')

    print(f"Aligned '{input_filepath}' (right-aligned) and saved to '{output_filepath}'")

# --- How to use the script ---
if __name__ == "__main__":
    # Set up argument parser
    parser = argparse.ArgumentParser(description='Align columns in a CSV file.')
    parser.add_argument('input_file', help='Path to the input unaligned CSV file.')
    parser.add_argument('output_file', help='Path to save the output aligned CSV file.')

    # Parse arguments from the command line
    args = parser.parse_args()

    # --- IMPORTANT ---
    # This script now lacks explicit error handling for file operations (e.g., FileNotFoundError).
    # Ensure the input file exists before running, or the script will crash.

    # Run the alignment function using the provided arguments
    align_csv(args.input_file, args.output_file)

    # Example command line usage:
    # python your_script_name.py unaligned_data.csv aligned_data.csv
