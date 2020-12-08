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

/** This test checks if sql SAVEPOINT statement works correctly. */
class TestSqlSavepointStatement {
  private static final String TEXT =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //    SAVEPOINT A ON ROLLBACK RETAIN CURSORS;
      //       ⋮
      //   SAVEPOINT B UNIQUE ON ROLLBACK RETAIN CURSORS;
      //       ⋮
      //   SAVEPOINT A ON ROLLBACK RETAIN CURSORS;
      ;

  @Test
  void test() {
    // UseCaseEngine.runTest(TEXT, List.of(), Map.of());
  }
}
