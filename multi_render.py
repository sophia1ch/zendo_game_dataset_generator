import subprocess
import sys, argparse
import os
import yaml
import glob
import csv
import re
import signal
sys.path.append(os.path.dirname(os.path.abspath(__file__)))
from argparse import Namespace

def extract_start_rule(filename):
    """Extract the numeric start_rule from a file like ground_truth_4.csv"""
    match = re.search(r"ground_truth_(\d+)\.csv", filename)
    return int(match.group(1)) if match else float('inf')

def merge_ground_truth_csvs(output_dir="output", output_filename="ground_truth.csv"):
    pattern = os.path.join(output_dir, "ground_truth_*.csv")
    csv_files = sorted(glob.glob(pattern), key=extract_start_rule)

    if not csv_files:
        print("No partial CSV files found.")
        return

    output_path = os.path.join(output_dir, output_filename)

    with open(output_path, "w", newline="") as outfile:
        writer = None
        for idx, csv_file in enumerate(csv_files):
            with open(csv_file, "r", newline="") as infile:
                reader = csv.reader(infile)
                rows = list(reader)

                if not rows:
                    continue

                if idx == 0:
                    # Write full contents including header
                    writer = csv.writer(outfile)
                    writer.writerows(rows)
                else:
                    # Skip header row
                    writer.writerows(rows[1:])

    print(f"Merged {len(csv_files)} files into {output_path}")

def run_blender_subprocess(start_rule, end_rule, config_file):
    return subprocess.Popen(
        [
            "blender", "--background", "--python", "render.py", "--",
            "--config-file", config_file,
            "--start-rule", str(start_rule),
            "--end-rule", str(end_rule)
        ],
        preexec_fn=os.setsid  # Start in a new process group
    )

def load_zendo_rules(filepath):
    if not os.path.exists(filepath):
        print(f"Error: The file {filepath} does not exist.")
        return None, None, None

    rules = []
    queries = []
    queries_n = []

    with open(filepath, "r", encoding="utf-8") as f:
        current_rule = None
        current_query = None
        current_query_n = None

        for line in f:
            line = line.strip()
            if not line or line.startswith("#"):
                continue

            if line.startswith("rule:"):
                current_rule = line.split("rule:", 1)[1].strip().strip("'")
                rules.append(current_rule)
            elif line.startswith("query:"):
                current_query = line.split("query:", 1)[1].strip().strip("'")
                queries.append(current_query)
            elif line.startswith("query_n:"):
                current_query_n = line.split("query_n:", 1)[1].strip().strip("'")
                queries_n.append(current_query_n)

    return rules, queries, queries_n

if __name__ == "__main__":
    config = "configs/simple_config.yml"
    parser = argparse.ArgumentParser()

    parser.add_argument("-c", "--config-file", type=str, default="configs/simple_config.yml",
                        help='config file for rendering')
    parser.add_argument("--processes", type=int, default=1,
                        help="number of processes to use for rendering")
    conf = parser.parse_args(sys.argv[sys.argv.index("--") + 1:])

    with open(conf.config_file) as f:
        args = yaml.safe_load(f.read())
    args = Namespace(**args)

    num_rules = args.num_rules
    rulesFile, q, qn = load_zendo_rules(args.zendo_rules_fixed_file)
    if rulesFile is not None:
        num_rules = len(rulesFile)

    print(f"Number of rules: {num_rules}")
    print(f"Number of processes: {conf.processes}")
    rules_per_process = num_rules // conf.processes
    print(f"Number of rules per process: {rules_per_process}")

    processes = []
    try:
        for p in range(conf.processes):
            start = p * rules_per_process
            end = start + rules_per_process - 1
            if p == conf.processes - 1:
                end = num_rules - 1

            print(f"Process {p}: start = {start}, end = {end}")
            proc = run_blender_subprocess(start, end, config)
            processes.append(proc)

        # Wait for processes to finish
        for proc in processes:
            proc.wait()

        print("All rendering subprocesses completed.")
        merge_ground_truth_csvs()

    except KeyboardInterrupt:
            print("\nInterrupted by user. Terminating all Blender processes...")
            for proc in processes:
                try:
                    proc.terminate()
                    proc.wait(timeout=5)
                except Exception as e:
                    print(f"Could not terminate process cleanly: {e}")
                    proc.kill()
            print("Cleanup complete. Exiting.")
            sys.exit(1)