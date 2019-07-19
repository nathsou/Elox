#!./util/env/bin/python3

# https://github.com/munificent/craftinginterpreters/blob/master/util/test.py

from __future__ import print_function

from os import listdir
from os.path import abspath, basename, dirname, isdir, isfile, join, realpath, relpath, splitext
import re
from subprocess import Popen, PIPE
import sys

ROOT_DIR = dirname(realpath(__file__))
TESTS_DIR = ROOT_DIR + "/tests"
ELOX_INTERP_PATH = ROOT_DIR + "/target/release/elox"

OUTPUT_EXPECT = re.compile(r'// ?!expect: ?(.*)')
RUNTIME_ERROR_EXPECT = re.compile(r'// ?!expect runtime error: (.*)')
SYNTAX_ERROR_EXPECT = re.compile(r'// ?!expect syntax error: (.*)')
ERROR_LINE = re.compile(r'Error: \[line ([0-9]+):([0-9]+)\]: (.+)')

expectations = 0


class EloxTest:
    def __init__(self, path):
        self.path = path
        self.output = []
        self.failures = []
        self.expectations = 0
        self.runtime_err = None
        self.syntax_err = None

    def parse(self):
        line_nb = 1
        with open(self.path, 'r') as file:
            for line in file:
                expect = OUTPUT_EXPECT.search(line)
                if expect:
                    self.output.append((expect.group(1), line_nb))
                    self.expectations += 1

                runtime_err = RUNTIME_ERROR_EXPECT.search(line)
                if runtime_err:
                    if self.runtime_err:
                        self.fail(
                            "Test error: Cannot expect multiple runtime errors")
                        return

                    self.runtime_err = (runtime_err.group(1), line_nb)
                    self.expectations += 1

                syntax_err = SYNTAX_ERROR_EXPECT.search(line)
                if syntax_err:
                    if self.syntax_err:
                        self.fail(
                            "Test error: Cannot expect multiple syntax errors")
                        return
                    self.syntax_err = (syntax_err.group(1), line_nb)
                    self.expectations += 1
            line_nb += 1

    def run(self):
        args = [ELOX_INTERP_PATH, self.path]
        proc = Popen(args, stdin=PIPE, stdout=PIPE, stderr=PIPE)
        out, err = proc.communicate()
        self.validate(out, err)

    def validate(self, out, err):

        if self.syntax_err and self.runtime_err:
            self.fail(
                "Test error: Cannot expect both syntax and runtime errors.")
            return

        out = out.decode("utf-8").replace('\r\n', '\n')
        err = err.decode("utf-8").replace('\r\n', '\n')

        err_lines = err.split('\n')

        if self.syntax_err != None:
            self.validate_syntax_error(err_lines)

        if self.runtime_err != None:
            self.validate_runtime_error(err_lines)

        if len(self.failures) == 0:
            self.validate_output(out)

    def validate_syntax_error(self, errors):
        if len(errors) < 2:
            self.fail('Expected syntax error "{0}" and got none.',
                      self.syntax_err[0])
            return

        match = ERROR_LINE.search(errors[0])

        if match.group(3) != self.syntax_err[0]:
            self.fail(
                'Expected runtime error "{0}" and got: "{1}"', self.syntax_err[0], match.group(3))

    def validate_runtime_error(self, errors):
        if len(errors) < 2:
            self.fail('Expected runtime error "{0}" and got none.',
                      self.runtime_err[0])
            return

        match = ERROR_LINE.search(errors[0])

        if match.group(3) != self.runtime_err[0]:
            self.fail(
                'Expected runtime error "{0}" and got: "{1}"', self.runtime_err[0], match.group(3))

    def validate_output(self, out):
        # Remove the trailing last empty line.
        out_lines = out.split('\n')
        if out_lines[-1] == '':
            del out_lines[-1]

        index = 0
        for line in out_lines:
            if sys.version_info < (3, 0):
                line = line.encode('utf-8')

            if index >= len(self.output):
                self.fail('Got output "{0}" when none was expected.', line)
            elif self.output[index][0] != line:
                self.fail('Expected output "{0}" on line {1}, got "{2}".',
                          self.output[index][0], self.output[index][1], line)
            index += 1

        while index < len(self.output):
            self.fail('Missing expected output "{0}" on line {1}.',
                      self.output[index][0], self.output[index][1])
            index += 1

    def fail(self, message, *args):
        if args:
            message = message.format(*args)
        self.failures.append(message)


def run_test(path):
    global expectations
    test = EloxTest(path)
    test.parse()
    test.run()
    if len(test.failures) != 0:
        print(relpath(path).replace("\\", "/") + ' failed: ')
        for failure in test.failures:
            print(failure)
            sys.exit(1)
    else:
        expectations += test.expectations


def run_tests(dir):
    global expectations
    prev_expectations = expectations
    expectations = 0

    for path in listdir(dir):
        sub_path = join(dir, path)
        if isdir(sub_path):
            run_tests(sub_path)
        else:
            run_test(sub_path)

    print('All tests passed in ' + relpath(dir) +
          ', expectations: ' + str(expectations))

    expectations += prev_expectations


run_tests(TESTS_DIR)
