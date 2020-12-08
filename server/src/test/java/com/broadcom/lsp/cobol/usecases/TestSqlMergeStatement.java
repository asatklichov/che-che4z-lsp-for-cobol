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

import com.broadcom.lsp.cobol.usecases.engine.UseCaseEngine;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

import java.util.List;
import java.util.Map;
import java.util.stream.Stream;

/** This test checks if sql MERGE statement works correctly. */
class TestSqlMergeStatement {
  private static final String MERGE1 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // MERGE INTO RECORDS AR
      // USING (SELECT ACTIVITY, DESCRIPTION FROM ACTIVITIES) AC
      // ON (AR.ACTIVITY = AC.ACTIVITY)
      // WHEN MATCHED THEN
      //  UPDATE SET
      //  DESCRIPTION = AC.DESCRIPTION
      // WHEN NOT MATCHED THEN
      //  INSERT
      //  (ACTIVITY, DESCRIPTION)
      //  VALUES (AC.ACTIVITY, AC.DESCRIPTION);
      ;

  private static final String MERGE2 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //  MERGE INTO INVENTORY AS IN
      // USING (SELECT PARTNO, DESCRIPTION, COUNT FROM SHIPMENT
      // WHERE SHIPMENT.PARTNO IS NOT NULL) AS SH
      // ON (IN.PARTNO = SH.PARTNO)
      // WHEN MATCHED THEN
      //  UPDATE SET
      //   DESCRIPTION = SH.DESCRIPTION,
      //   QUANTITY = IN.QUANTITY + SH.COUNT
      // WHEN NOT MATCHED THEN
      //  INSERT
      //  (PARTNO, DESCRIPTION, QUANTITY)
      //  VALUES (SH.PARTNO, SH.DESCRIPTION, SH.COUNT);
      ;

  private static final String MERGE3 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // MERGE INTO ACCOUNT AS A
      // USING (SELECT ID, SUM(AMOUNT) SUM_AMOUNT FROM TRANSACTION
      //  GROUP BY ID) AS T
      //  ON A.ID = T.ID
      // WHEN MATCHED THEN
      //  UPDATE SET
      //   BALANCE = A.BALANCE + T.SUM_AMOUNT
      // WHEN NOT MATCHED THEN
      //  INSERT
      //  (ID, BALANCE)
      //   VALUES (T.ID, T.SUM_AMOUNT);
      ;

  private static final String MERGE4 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // MERGE INTO EMPLOYEE_FILE AS E
      // USING (SELECT EMPID, PHONE, OFFICE
      //  FROM (SELECT EMPID, PHONE, OFFICE,
      //  ROW_NUMBER() OVER (PARTITION BY EMPID
      //  ORDER BY TRANSACTION_TIME DESC) RN
      //  FROM TRANSACTION_LOG) AS NT
      //  WHERE RN = 1) AS T
      //  ON E.EMPID = T.EMPID
      // WHEN MATCHED THEN
      //  UPDATE SET
      //   (PHONE, OFFICE) =
      //   (T.PHONE, T.OFFICE)
      // WHEN NOT MATCHED THEN
      //  INSERT
      //  (EMPID, PHONE, OFFICE)
      //  VALUES (T.EMPID, T.PHONE, T.OFFICE);
      ;

  private static final String MERGE5 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // MERGE INTO RECORDS AR
      // USING (SELECT ACTIVITY, DESCRIPTION, DATE, LAST_MODIFIED
      //  FROM ACTIVITIES_GROUPA) AC
      //  ON (AR.ACTIVITY = AC.ACTIVITY) AND AR.GROUP = 'A'
      // WHEN MATCHED AND AC.DATE IS NULL THEN
      //  SIGNAL SQLSTATE '70001'
      //   SET MESSAGE_TEXT =
      //    AC.ACTIVITY CONCAT ' CANNOT BE MODIFIED. REASON: DATE IS NOT KNOWN'
      // WHEN MATCHED AND AC.DATE < CURRENT DATE THEN
      //  DELETE
      // WHEN MATCHED AND AR.LAST_MODIFIED < AC.LAST_MODIFIED THEN
      //  UPDATE SET
      //  (DESCRIPTION, DATE, LAST_MODIFIED) = (AC.DESCRIPTION, AC.DATE, DEFAULT)
      // WHEN NOT MATCHED AND AC.DATE IS NULL THEN
      //  SIGNAL SQLSTATE '70002'
      //   SET MESSAGE_TEXT =
      //    AC.ACTIVITY CONCAT ' CANNOT BE INSERTED. REASON: DATE IS NOT KNOWN'
      // WHEN NOT MATCHED AND AC.DATE >= CURRENT DATE THEN
      //  INSERT
      //   (GROUP, ACTIVITY, DESCRIPTION, DATE)
      //   VALUES ('A', AC.ACTIVITY, AC.DESCRIPTION, AC.DATE)
      // ELSE IGNORE;
      ;

  private static final String MERGE6 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // MERGE INTO RECORDS AR
      //  USING (VALUES (:hv_activity, :hv_description)
      //    FOR :hv_nrows ROWS)
      //    AS AC (ACTIVITY, DESCRIPTION)
      //  ON (AR.ACTIVITY = AC.ACTIVITY)
      //  WHEN MATCHED THEN UPDATE SET DESCRIPTION = AC.DESCRIPTION
      //  WHEN NOT MATCHED THEN INSERT (ACTIVITY, DESCRIPTION)
      //     VALUES (AC.ACTIVITY, AC.DESCRIPTION)
      //  NOT ATOMIC CONTINUE ON SQLEXCEPTION;
      ;

  private static final String MERGE7 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // MERGE INTO ACCOUNT AS A
      //  USING (VALUES (:hv_id, :hv_amount)
      //    FOR 3 ROWS)
      //    AS T (ID, AMOUNT)
      //  ON (A.ID = T.ID)
      //  WHEN MATCHED THEN UPDATE SET BALANCE = A.BALANCE + T.AMOUNT
      //  WHEN NOT MATCHED THEN INSERT (ID, BALANCE)
      //      VALUES (T.ID, T.AMOUNT)
      //  NOT ATOMIC CONTINUE ON SQLEXCEPTION;
      ;

  private static Stream<String> textsToTest() {
    // add all
    return Stream.of(MERGE1, MERGE2, MERGE3, MERGE4, MERGE5);
  }

  @ParameterizedTest
  @MethodSource("textsToTest")
  @DisplayName("Parameterized - sql create statements tests")
  void test(String text) {
    UseCaseEngine.runTest(text, List.of(), Map.of());
  }
}
