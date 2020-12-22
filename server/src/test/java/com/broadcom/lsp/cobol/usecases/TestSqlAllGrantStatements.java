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

/**
 * This PARAMETERIZED test checks if all below sql GRANT statements works correctly.
 *
 * <pre>
 *     - Collection
 *     - Database
 *     - Function or stored procedure
 *     - Package
 *     - Plan
 *     - Schema
 *     - Sequence
 *     - System
 *     - Table or view
 *     - Type or JAR
 *     - Variable
 *     - Use
 * </pre>
 */
class TestSqlAllGrantStatements {
  private static final String GRANT_ON_COLLECTION =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //  GRANT CREATE IN COLLECTION DSN8CC91 TO ROLE ROLE1;
      ;

  private static final String GRANT_ON_COLLECTION2 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //  GRANT CREATE IN COLLECTION DSN8CC91 TO ROLE ROLE1;
      ;

  private static final String GRANT_ON_DATABASE =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // GRANT DROP
      //     ON DATABASE DSN8D12A
      //     TO PEREZ;
      ;

  private static final String GRANT_ON_DATABASE2 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //  GRANT REPAIR
      //     ON DATABASE DSN8D12A
      //     TO PUBLIC;
      ;

  private static final String GRANT_ON_DATABASE3 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //  GRANT CREATETAB,LOAD
      //     ON DATABASE DSN8D12A
      //     TO WALKER,PIANKA,FUJIMOTO
      //     WITH GRANT OPTION;
      ;

  private static final String GRANT_ON_DATABASE4 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // GRANT LOAD
      //     ON DATABASE DSN9D91A
      //     TO ROLE ROLE1;
      ;

  private static final String GRANT_ON_FUNCTION =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // GRANT EXECUTE ON FUNCTION CALC_SALARY TO JONES;
      ;

  private static final String GRANT_ON_SPECIFIC_FUNCTION =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // GRANT EXECUTE ON SPECIFIC FUNCTION DEPT85_TOT TO ADMIN_A
      //         WITH GRANT OPTION;
      ;

  private static final String GRANT_ON_PROCEDURE =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // GRANT EXECUTE ON PROCEDURE VACATION_ACCR TO PUBLIC;
      ;

  private static final String GRANT_ON_PACKAGE =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // GRANT COPY ON PACKAGE DSN8CC61.* TO LEWIS;
      ;

  private static final String GRANT_ON_PACKAGE2 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // GRANT ALL ON PACKAGE CLCT1.PKG1, CLCT2.PKG2 TO JONES;
      ;

  private static final String GRANT_ON_PACKAGE3 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // GRANT EXECUTE ON PACKAGE DSN9CC13.* TO ROLE ROLE1;
      ;

  private static final String GRANT_ON_PLAN =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //  GRANT BIND ON PLAN DSN8IP12 TO JONES;
      ;

  private static final String GRANT_ON_PLAN2 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // GRANT BIND,EXECUTE ON PLAN DSN8CP12 TO PUBLIC;
      ;

  private static final String GRANT_ON_PLAN3 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //  GRANT EXECUTE ON PLAN DSN8CP12 TO ADAMSON,BROWN WITH GRANT OPTION;
      ;

  private static final String GRANT_ON_PLAN4 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // GRANT BIND ON PLAN DSN91PLN TO ROLE ROLE1;
      ;

  private static final String GRANT_ON_SCHEMA =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // GRANT CREATEIN ON SCHEMA T_SCORES TO JONES;
      ;

  private static final String GRANT_ON_SCHEMA2 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // GRANT ALTERIN ON SCHEMA DEPT TO ADMIN_A
      //         WITH GRANT OPTION;
      ;

  private static final String GRANT_ON_SCHEMA3 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // GRANT CREATEIN, ALTERIN, DROPIN ON SCHEMA NEW_HIRE, PROMO, RESIGN TO HR;
      ;

  private static final String GRANT_ON_SCHEMA4 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // GRANT ALTERIN ON SCHEMA EMPLOYEE TO ROLE ROLE1;
      ;

  private static final String GRANT_ON_SEQUENCE =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // GRANT USAGE
      //     ON SEQUENCE MYNUM
      //     TO JONES;
      ;

  private static final String GRANT_ON_SEQUENCE2 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //    GRANT USAGE ON SEQUENCE ORDER_SEQ TO ROLE ROLE1;
      ;

  private static final String GRANT_SYS_PRIVILGES =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // GRANT DISPLAY
      //     TO LUTZ;
      ;

  private static final String GRANT_SYS_PRIVILGES2 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //    GRANT BSDS,RECOVER
      //     TO PARKER,SETRIGHT
      //     WITH GRANT OPTION;
      ;

  private static final String GRANT_SYS_PRIVILGES3 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //    GRANT TRACE
      //     TO PUBLIC;
      ;

  private static final String GRANT_SYS_PRIVILGES4 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //    GRANT ARCHIVE TO ROLE ROLE1;
      ;

  private static final String GRANT_SYS_PRIVILGES5 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //    GRANT CREATE_SECURE_OBJECT
      //		TO STEVE;
      ;

  private static final String GRANT_SYS_PRIVILGES6 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //    GRANT DBADM ON SYSTEM
      //		TO ROLE ADMINROLE;
      //	GRANT DBADM, ACCESSCTRL, DATAACCESS
      //		ON SYSTEM
      //		TO SALLY;
      ;

  private static final String GRANT_SYS_PRIVILGES7 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //    GRANT DBADM WITHOUT ACCESSCTRL
      //		WITHOUT DATAACCESS
      //		ON SYSTEM
      //		TO JOHN;
      ;

  private static final String GRANT_ON_TABLE =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //    GRANT SELECT ON DSN8C10.EMP TO PULASKI;
      ;

  private static final String GRANT_ON_TABLE2 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //    GRANT ALL ON TABLE DSN8C10.EMP TO KWAN,THOMPSON WITH GRANT OPTION;
      ;

  private static final String GRANT_ON_TABLE3 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //    GRANT ALTER ON TABLE DSN9910.EMP TO ROLE ROLE1;
      ;

  private static final String GRANT_ON_TYPE =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //    GRANT USAGE ON TYPE SHOE_SIZE TO JONES;
      ;

  private static final String GRANT_ON_TYPE2 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //   GRANT USAGE ON TYPE US_DOLLAR TO PUBLIC;
      ;

  private static final String GRANT_ON_TYPE3 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //    GRANT USAGE ON TYPE MILES
      //    TO ROLE ROLE1;
      ;

  private static final String GRANT_ON_VARIABLE =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //  GRANT READ ON VARIABLE ACCOUNTNO TO JONES;
      ;

  private static final String GRANT_ON_VARIABLE2 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //  GRANT USE OF TABLESPACE
      //     DSN8D12A.DSN8S12D
      //     TO PUBLIC;
      ;

  private static final String GRANT_ON_VARIABLE3 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // GRANT USE OF STOGROUP SG1
      //      TO ROLE ROLE1;
      ;

  private static Stream<String> textsToTest() {
    // add all
    return Stream.of(GRANT_ON_COLLECTION, GRANT_ON_COLLECTION2, GRANT_ON_DATABASE);
  }

  @ParameterizedTest
  @MethodSource("textsToTest")
  @DisplayName("Parameterized - sql all grant statements tests")
  void test(String text) {
    UseCaseEngine.runTest(text, List.of(), Map.of());
  }
}
