#/usr/bin/python3
import argparse
from collections import Counter
import itertools
import re
import subprocess
import sys


if sys.version_info[0] < 3:
    raise Exception("Must be using Python 3")


def is_timestamp(aString):
    return re.fullmatch(r"\d\d\d\d-\d\d-\d\d \d\d:\d\d:\d\d", aString)


def strip_heading(lines):
    ok = is_timestamp(lines[0]) and lines[1].startswith("Full thread dump OpenJDK") and lines[2] == ""
    if not ok:
        raise Exception("Unknown jstack output heading!")
    return lines[3:]


METHOD_REGEX = r"\.([^.(]+\.[^.(]+)\([^()]+\)$"


def are_thread_lines(lines):
    return len(lines) >= 2 and lines[0].startswith('"') and lines[1].startswith("   java.lang.Thread.State:")


STACK_TRACE_LINE_PREFIX = "\tat "


def is_stack_trace_line(line):
    return line.startswith(STACK_TRACE_LINE_PREFIX)


def extract_method(line):
    match = re.search(METHOD_REGEX, line)
    if match:
        return match.group(1)
    else:
        raise Exception("ERORR: Could not match method. Line={}".format(line))


TOP_METHOD = "<TOP>"


def parse_thread(lines):
    thread_name = lines[0].split('"')[1]
    full_stack_trace = lines[2:]
    thread_lines = list(filter(is_stack_trace_line, full_stack_trace))
    if thread_lines:
        thread_lines[0] = (TOP_METHOD, thread_lines[0])
        for i in range(len(thread_lines) - 1):
            thread_lines[i + 1] = (extract_method(thread_lines[i][1]), thread_lines[i + 1])
    return (thread_name, thread_lines, full_stack_trace)


def split_by_endlines(lines):
    return (list(group) for key, group in itertools.groupby(lines, key=lambda line: line != "") if key)


def parse_threads(lines):
    return map(parse_thread, filter(are_thread_lines, split_by_endlines(lines)))


def filter_by(prefix, threads):
    return (thread for thread in threads if thread[0].startswith(prefix))


def get_threads_sample(pid, prefix):
    result = subprocess.run(["jstack", str(pid)], stdout=subprocess.PIPE).stdout.decode("utf8")
    return filter_by(prefix, parse_threads(strip_heading(result.splitlines())))


def callsite_string(callsite):
    if callsite[0] == TOP_METHOD:
        return "{}\t RUNNING".format(callsite[1])
    else:
        return "{}\tCALLING {}".format(callsite[1], callsite[0])

def main(pid, samples, prefix, package, hide_stack_traces):
    if samples < 1:
        raise Exception("ERROR: At least one sample is required")
    jvm_threads = itertools.chain(*[get_threads_sample(pid, prefix) for _i in range(samples)])
    total_by_callsite = Counter()
    total_threads = 0
    for name, thread, full_stack_trace in jvm_threads:
        total_threads += 1
        if not hide_stack_traces:
            print("THREAD : {}".format(name))
            for line in full_stack_trace:
                print(line)
            print()
        for callsite in set(thread):
            if callsite[1].startswith(STACK_TRACE_LINE_PREFIX + package):
                total_by_callsite[callsite] += 1
    for callsite, count in total_by_callsite.most_common():
        print("{:5.1f}%{}".format(100.0 * float(count) / float(total_threads), callsite_string(callsite)))

if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="Find critical function calls by sampling jvm stack traces, to aid in performance tuning.")
    parser.add_argument("pid", metavar="pid", type=int,
                        help="Process id of a running java virtual machine")
    parser.add_argument("--samples", dest="samples", default=1, metavar="N", type=int,
                        help="Discard threads whose name does not start with prefix")
    parser.add_argument("--thread-prefix", dest="prefix", default="", metavar="prefix", type=str,
                        help="Discard threads whose name does not start with prefix")
    parser.add_argument("--package", dest="package", default="", metavar="package-name", type=str,
                        help="When counting, consider only stack trace entries within the given package")
    parser.add_argument("--hide-stack-traces", dest="hide_stack_traces", action='store_true',
                        help="When counting, consider only stack trace entries within the given package")

    args = parser.parse_args()
    main(args.pid, args.samples, args.prefix, args.package, args.hide_stack_traces)
