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

/** This test checks if sql SIGNAL statement works correctly. */
class TestSqlSignalStatement {
  private static final String TEXT =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // CREATE PROCEDURE SUBMIT_ORDER
      //            (IN ONUM INTEGER, IN CNUM INTEGER,
      //             IN PNUM INTEGER, IN QNUM INTEGER)
      // LANGUAGE SQL
      // SPECIFIC SUBMIT_ORDER
      // MODIFIES SQL DATA
      // BEGIN
      //   DECLARE EXIT HANDLER FOR SQLSTATE VALUE '23503'
      //     SIGNAL SQLSTATE '75002'
      //         SET MESSAGE_TEXT = 'Customer number is not known';
      //   INSERT INTO ORDERS (ORDERNO, CUSTNO, PARTNO, QUANTITY)
      //      VALUES (ONUM, CNUM, PNUM, QNUM);
      // END
      ;

  @Test
  void test() {
    // UseCaseEngine.runTest(TEXT, List.of(), Map.of());
  }
}
