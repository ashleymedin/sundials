#!/usr/bin/env python3
# -----------------------------------------------------------------------------
# Programmer(s): Cody Balos @ LLNL
# -----------------------------------------------------------------------------
# SUNDIALS Copyright Start
# Copyright (c) 2002-2024, Lawrence Livermore National Security
# and Southern Methodist University.
# All rights reserved.
#
# See the top-level LICENSE and NOTICE files for details.
#
# SPDX-License-Identifier: BSD-3-Clause
# SUNDIALS Copyright End
# -----------------------------------------------------------------------------
# Library of Python functions that may be useful to SUNDIALS developers writing
# scripts to parse logs, statistics, etc.
# -----------------------------------------------------------------------------

import re

def parse_logfile_payload(payload):
  kvpstrs = payload.split(',')
  kvp_dict = {}
  for kvpstr in kvpstrs:
    kvp = kvpstr.split('=')
    if len(kvp) == 1:
      kvp_dict[kvp[0].strip()] = ''
    else:
      key, value = kvp
      kvp_dict[key.strip()] = value.strip()
  return kvp_dict

def parse_logfile_line(line):
  pattern = re.compile('\[(\w+)\]\[(rank \d+)\]\[(.*)\]\[(.*)\](.*)')
  matches = pattern.findall(line)
  line_dict = {}
  if matches:
    line_dict['loglvl'] = matches[0][0]
    line_dict['rank'] = matches[0][1]
    line_dict['scope'] = matches[0][2]
    line_dict['label'] = matches[0][3]
    line_dict['payload'] = parse_logfile_payload(matches[0][4])
  return line_dict

def cvode_debug_file_to_list(filename):
  """
  This function takes a debug log file from CVODE and creates a list where
  each list entry is a step attempt.
  """

  with open(filename, "r") as logfile:
    log = []
    lines_for_this_step = None
    for line in logfile:
      line_dict = parse_logfile_line(line.rstrip())
      if not line_dict:
        continue
      if line_dict['scope'] == 'CVODE::cvStep' and line_dict['label'] == 'enter-step-attempt-loop':
        if lines_for_this_step is None:
          lines_for_this_step = [line_dict]
        else:
          log.append(lines_for_this_step)
          lines_for_this_step =[line_dict]
      else:
        lines_for_this_step.append(line_dict)
    return log
