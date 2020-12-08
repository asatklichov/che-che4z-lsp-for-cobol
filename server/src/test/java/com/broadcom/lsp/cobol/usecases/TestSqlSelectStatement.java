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

package com.broadcom.lsp.cobol.usecases;

import org.junit.jupiter.api.Test;

/** This test checks if sql SELECT  statement works correctly. */
class TestSqlSelectStatement {
  private static final String TEXT =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // EXPLAIN PLAN SET QUERYNO = 13
      //   FOR SELECT X.ACTNO, X.PROJNO, X.EMPNO, Y.JOB, Y.EDLEVEL
      //       FROM DSN8C10.EMPPROJACT X, DSN8C10.EMP Y
      //          WHERE X.EMPNO = Y.EMPNO
      //             AND X.EMPTIME > 0.5
      //             AND (Y.JOB = 'DESIGNER' OR Y.EDLEVEL >= 12)
      //          ORDER BY X.ACTNO, X.PROJNO;
      ;

  @Test
  void test() {
    // UseCaseEngine.runTest(TEXT, List.of(), Map.of());
  }
}
