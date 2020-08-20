/*
 * Copyright (c) 2020 Broadcom.
 * The term "Broadcom" refers to Broadcom Inc. and/or its subsidiaries.
 *
 * This program and the accompanying materials are made
 * available under the terms of the Eclipse Public License 2.0
 * which is available at https://www.eclipse.org/legal/epl-2.0/
 *
 * SPDX-License-Identifier: EPL-2.0
 *
 * Contributors:
 *    Broadcom, Inc. - initial API and implementation
 *
 */

package com.ca.lsp.cobol.usecases;

import com.ca.lsp.cobol.positive.CobolText;
import com.ca.lsp.cobol.usecases.engine.UseCaseEngine;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Map;

/**
 * Test replacing by an empty pattern. Here, 'TAG-' and 'TAR-' should be removed while the copybook
 * processing, leaving only PID and PKEY respectively.
 */
class TestReplacingByEmptyPseudoText {

  private static final String TEXT =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. TESTREPL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
          + "       01  {$*PARENT}.\n"
          + "       COPY {~REPL} REPLACING == TAG- == BY ====\n"
          + "                              == TAR- == BY == ==.\n"
          + "       PROCEDURE DIVISION.\n"
          + "       {#*MAINLINE}.\n"
          + "           MOVE 0 TO {$PID} OF {$PARENT}.\n"
          + "           MOVE 0 TO {$PKEY} OF {$PARENT}.\n"
          + "           GOBACK. ";

  private static final String REPL =
      "           10 {$*TAG-PID^PID} PIC 9.\n" + "           10 {$*TAR-PKEY^PKEY} PIC 9.\n";

  private static final String REPL_NAME = "REPL";

  @Test
  void test() {
    UseCaseEngine.runTest(TEXT, List.of(new CobolText(REPL_NAME, REPL)), Map.of());
  }
}
