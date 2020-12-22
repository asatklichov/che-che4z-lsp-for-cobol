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
 * This PARAMETERIZED test checks if all below sql REVOKE statements works correctly.
 *
 * <pre>
 * - Collection
 * - Database
 * - Function or stored procedure
 * - Package
 * - Plan
 * - Schema
 * - Sequence
 * - System
 * - Table or view
 * - Type or JAR file
 * - Variable
 * Use
 *
 * </pre>
 */
class TestSqlAllRevokeStatements {
  private static final String REVOKE_IN_COLLECTION =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //    REVOKE CREATE IN COLLECTION QAACLONE, DSN8CC61 FROM CLARK;
      ;

  private static final String REVOKE_IN_COLLECTION2 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //    REVOKE CREATE IN COLLECTION DSN8CC91 FROM ROLE ROLE1;
      ;

  private static final String REVOKE_ON_DATABASE =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //    REVOKE DROP
      //     ON DATABASE DSN8D12A
      //     FROM PEREZ;
      ;

  private static final String REVOKE_ON_DATABASE2 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //     REVOKE REPAIR
      //     ON DATABASE DSN8D12A
      //     FROM PUBLIC;
      ;

  private static final String REVOKE_ON_DATABASE3 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //     REVOKE CREATETAB,LOAD
      //     ON DATABASE DSN8D12A
      //     FROM WALKER,PIANKA,FUJIMOTO;
      ;

  private static final String REVOKE_ON_DATABASE4 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // REVOKE LOAD
      //     ON DATABASE DSN9D91A
      //     TO ROLE ROLE1;
      ;

  private static final String REVOKE_ON_FUNCTION =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //  REVOKE EXECUTE ON FUNCTION CALC_SALARY FROM JONES;
      ;

  private static final String REVOKE_ON_PROCEDURE =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // REVOKE EXECUTE ON PROCEDURE VACATION_ACCR FROM PUBLIC;
      ;

  private static final String REVOKE_ON_PACKAGE =
          "       IDENTIFICATION DIVISION.\n"
                  + "       PROGRAM-ID. HELLO-SQL.\n"
                  + "       DATA DIVISION.\n"
                  + "       WORKING-STORAGE SECTION.\n"
          //  REVOKE COPY ON PACKAGE DSN8CC61.* FROM LEWIS;
          ;

  private static final String REVOKE_ON_PACKAGE2 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //   REVOKE EXECUTE ON PACKAGE DSN9CC13.* FROM ROLE ROLE1;
      ;

  private static final String REVOKE_ON_PACKAGE3 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // REVOKE EXECUTE ON PACKAGE DSN9CC13.* TO ROLE ROLE1;
      ;

  private static final String REVOKE_ON_PLAN =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // REVOKE BIND ON PLAN DSN8IP12 FROM JONES;
      ;

  private static final String REVOKE_ON_PLAN2 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // REVOKE BIND,EXECUTE ON PLAN DSN8CP12 FROM PUBLIC;
      ;

  private static final String REVOKE_ON_PLAN3 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //   REVOKE EXECUTE ON PLAN DSN8CP12 FROM ADAMSON,BROWN;
      ;

  private static final String REVOKE_ON_PLAN4 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //   REVOKE BIND ON PLAN DSN91PLN FROM ROLE ROLE1;
      ;

  private static final String REVOKE_ON_SCHEMA =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //   REVOKE CREATEIN ON SCHEMA T_SCORES FROM JONES;
      ;

  private static final String REVOKE_ON_SCHEMA2 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //    REVOKE CREATEIN ON SCHEMA VAC FROM PUBLIC;
      ;

  private static final String REVOKE_ON_SCHEMA3 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //   REVOKE ALTERIN ON SCHEMA DEPT FROM ADMIN_A;
      ;

  private static final String REVOKE_ON_SCHEMA4 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // REVOKE ALTERIN ON SCHEMA EMPLOYEE TO ROLE ROLE1;
      ;

  private static final String REVOKE_ON_SCHEMA5 =
          "       IDENTIFICATION DIVISION.\n"
                  + "       PROGRAM-ID. HELLO-SQL.\n"
                  + "       DATA DIVISION.\n"
                  + "       WORKING-STORAGE SECTION.\n"
          //   REVOKE ALTERIN ON SCHEMA EMPLOYEE FROM ROLE ROLE1;
          ;

  private static final String REVOKE_ON_SEQUENCE =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //   REVOKE USAGE
      //     ON SEQUENCE MYNUM
      //     FROM JONES;
      ;

  private static final String REVOKE_ON_SEQUENCE2 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //   REVOKE USAGE
      //     ON SEQUENCE ORDER_SEQ
      //     FROM ROLE ROLE1;
      ;

  private static final String REVOKE_SYS_PRIVILGES =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //   REVOKE DISPLAY
      //     FROM LUTZ;
      ;

  private static final String REVOKE_SYS_PRIVILGES2 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //   REVOKE BSDS,RECOVER
      //     FROM PARKER,SETRIGHT;
      ;

  private static final String REVOKE_SYS_PRIVILGES3 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //   REVOKE TRACE
      //     FROM PUBLIC;
      ;

  private static final String REVOKE_SYS_PRIVILGES4 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //   REVOKE ARCHIVE
      //     FROM ROLE ROLE1;
      ;

  private static final String REVOKE_SYS_PRIVILGES5 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //   REVOKE CREATE_SECURE_OBJECT
      //		 FROM STEVE BY MARY;
      ;

  private static final String REVOKE_SYS_PRIVILGES6 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //   REVOKE DBADM ON SYSTEM
      //		FROM ROLE ADMINROLE
      //		NOT INCLUDING DEPENDENT PRIVILEGES;
      ;

  private static final String REVOKE_SYS_PRIVILGES7 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //   REVOKE DBADM, DATAACCESS, ACCESSCTRL ON SYSTEM
      //		FROM ROLE ADMINROLE
      //		NOT INCLUDING DEPENDENT PRIVILEGES;
      ;

  private static final String REVOKE_ON_TABLE =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //   REVOKE SELECT ON TABLE DSN8C10.EMP FROM PULASKI;
      ;

  private static final String REVOKE_ON_TABLE2 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //   REVOKE UPDATE ON TABLE DSN8C10.EMP FROM PUBLIC;
      ;

  private static final String REVOKE_ON_TABLE3 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //   REVOKE ALL ON TABLE DSN8C10.EMP FROM KWAN,THOMPSON;
      ;

  private static final String REVOKE_ON_TABLE4 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //   REVOKE SELECT, UPDATE ON TABLE DSN8C10.DEPT
      //     FROM PUBLIC;
      ;

  private static final String REVOKE_ON_TABLE5 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //   REVOKE ALTER ON TABLE DSN8C10.EMP
      //     FROM ROLE ROLE1;
      ;

  private static final String REVOKE_ON_TYPE =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //   REVOKE USAGE ON TYPE SHOESIZE FROM JONES;
      ;

  private static final String REVOKE_ON_TYPE2 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //   REVOKE USAGE ON TYPE US_DOLLAR FROM PUBLIC;
      ;

  private static final String REVOKE_ON_TYPE3 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //   REVOKE USAGE ON TYPE CANADIAN_DOLLARS
      //         FROM ADMIN_A;
      ;

  private static final String REVOKE_ON_TYPE4 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //   REVOKE USAGE ON TYPE MILES
      //         FROM ROLE ROLE1;
      ;

  private static final String REVOKE_ON_VARIABLE =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // ?
      ;

  private static final String REVOKE_USE =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //   REVOKE USE OF BUFFERPOOL BP2
      //     FROM MARINO;
      ;

  private static final String REVOKE_USE2 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //   REVOKE USE OF TABLESPACE DSN8D12A.DSN8S12D
      //     FROM PUBLIC;
      ;

  private static final String REVOKE_USE3 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //   REVOKE USE OF STOGROUP SG1
      //     FROM ROLE ROLE1;
      ;

  private static Stream<String> textsToTest() {
    // add all
    return Stream.of(REVOKE_ON_DATABASE);
  }

  @ParameterizedTest
  @MethodSource("textsToTest")
  @DisplayName("Parameterized - sql revoke statements tests")
  void test(String text) {
    UseCaseEngine.runTest(text, List.of(), Map.of());
  }
}
