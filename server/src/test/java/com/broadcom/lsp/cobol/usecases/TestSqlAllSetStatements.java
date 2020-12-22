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

/**
 * This PARAMETERIZED test checks if all below sql SET statements works correctly.
 *
 * <pre>
 * - SET CONNECTION
 * - SET assignment statement
 * - SET CURRENT ACCELARATOR
 * - SET CURRENT APPLICATION COMPATIBILITY
 * - SET CURRENT ENCODING SCHEMA
 * - SET CURRENT DEBUG
 * - SET CURRENT DECFLOAT ROUNDING MODE
 * - SET CURRENT DEGREE
 * - SET CURRENT EXPLAIN MODE
 * - SET CURRENT GET_ACCEL_ARCHIVE
 * - SET CURRENT LOCALE LC_CTYPE
 * - SET CURRENT MAINTAINED TABLE TYPES FOR OPTIMIZATION
 * - SET CURRENT OPTIMIZATION HINT
 * - SET CURRENT PACKAGE PATH
 * - SET CURRENT PACKAGESET
 * - SET CURRENT PRECISION
 * - SET CURRENT QUERY ACCELERATION
 * - SET CURRENT QUERY ACCELERATION WAITFORDATA
 * - SET CURRENT REFRESH AGE
 * - SET CURRENT ROUTINE VERSION
 * - SET CURRENT RULES
 * - SET CURRENT SQLID
 * - SET CURRENT TEMPORAL BUSINESS_TIME
 * - SET CURRENT TEMPORAL SYSTEM_TIME
 * - SET ENCRYPTION PASSWORD
 * - SET PATH
 * - SET SCHEMA
 * - SET SESSION TIME ZONE
 * </pre>
 */
class TestSqlAllSetStatements {
  private static final String SET_CONNECTION =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //  EXEC SQL CONNECT TO TOROLAB1;
      //
      //    -- execute statements referencing objects at TOROLAB1
      //
      //  EXEC SQL CONNECT TO TOROLAB2;
      //
      //    -- execute statements referencing objects at TOROLAB2
      //
      //  EXEC SQL SET CONNECTION TOROLAB1;
      //
      //    -- execute statements referencing objects at TOROLAB1
      ;

  private static final String SET_ASSIGNMENT_STATE =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //   SET :HVL = CURRENT PATH;
      ;

  private static final String SET_ASSIGNMENT_STATE2 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //   SET :SERVER = CURRENT PATH,
      //       :XTIME = CURRENT TIME,
      //       :MEM = CURRENT MEMBER;
      ;

  private static final String SET_ASSIGNMENT_STATE3 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //   SET :DETAILS = SUBSTR(:LOCATOR,1,35);
      ;

  private static final String SET_ASSIGNMENT_STATE4 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //   SELECT SUBSTR(:LOCATOR,1,35)
      //      INTO :DETAILS
      //      FROM SYSIBM.SYSDUMMYU;
      ;

  private static final String SET_ASSIGNMENT_STATE5 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //   SET (SALARY, COMMISSION) = (50000, 8000);
      ;

  private static final String SET_CURRENT_ACCELARATOR_ =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // SET CURRENT ACCELERATOR = ACCEL1;
      ;

  private static final String SET_CURRENT_APPLICATION_COMPATIBILITY =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // EXEC SQL SET CURRENT APPLICATION COMPATIBILITY = 'V11R1';
      //  EXEC SQL SET CURRENT APPLICATION COMPATIBILITY = :HV1;
      ;

  private static final String SET_CURRENT_ENCODING_SCHEMA =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //  EXEC SQL SET CURRENT APPLICATION ENCODING SCHEME = 'EBCDIC';
      //  EXEC SQL SET CURRENT ENCODING SCHEME  = :HV1;
      ;

  private static final String SET_CURRENT_DEBUG_MODE =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //   SET CURRENT DEBUG MODE = ALLOW;
      ;

  private static final String SET_CURRENT_DECFLOAT_ROUNDING_MODE =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //   SET CURRENT DECFLOAT ROUNDING MODE = ROUND_CEILING;
      ;

  private static final String SET_CURRENT_DEGREE =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //   SET CURRENT DEGREE = '1';
      ;

  private static final String SET_CURRENT_DEGREE2 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //   SET CURRENT DEGREE = 'ANY';
      ;

  private static final String SET_CURRENT_EXPLAIN_MODE =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //   SET CURRENT EXPLAIN MODE = YES;
      ;
  private static final String SET_CURRENT_GET_ACCEL_ARCHIVE =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //  SET CURRENT GET_ACCEL_ARCHIVE=NO;
      ;

  private static final String SET_CURRENT_LOCALE_LC_CTYPE =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //   EXEC SQL SET CURRENT LOCALE LC_CTYPE = 'En_US';
      ;

  private static final String SET_CURRENT_LOCALE_LC_CTYPE2 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //   EXEC SQL SET CURRENT LOCALE LC_CTYPE = :HV1;
      ;

  private static final String SET_CURRENT_MAINTAINED_TABLE =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //   EXEC SQL SET CURRENT LOCALE LC_CTYPE = :HV1;
      ;

  private static final String SET_CURRENT_MAINTAINED_TABLE2 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //   SET CURRENT MAINTAINED TABLE TYPES ALL;
      ;
  private static final String SET_CURRENT_MAINTAINED_TABLE3 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //   EXEC SQL SET CURRENT LOCALE LC_CTYPE = :HV1;
      ;

  private static final String SET_CURRENT_OPTIMIZATION_HINT =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //   SET CURRENT OPTIMIZATION HINT = 'NOHYB';
      ;

  private static final String SET_CURRENT_OPTIMIZATION_HINT2 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //   SET CURRENT OPTIMIZATION HINT = '';
      ;

  private static final String SET_CURRENT_PACKAGE_PATH =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //   SET CURRENT PACKAGE PATH :hvar1;
      ;

  private static final String SET_CURRENT_PACKAGE_PATH2 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //  SET CURRENT PACKAGE PATH = "COLL1","COLL#2","COLL3", :hvar1;
      ;

  private static final String SET_CURRENT_PACKAGE_PATH3 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //  SET CURRENT PACKAGE PATH = ' ';
      ;

  private static final String SET_CURRENT_PACKAGESET =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //  EXEC SQL SET CURRENT PACKAGESET = 'PERSONNEL';
      ;

  private static final String SET_CURRENT_PACKAGESET2 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //  EXEC SQL SET CURRENT PACKAGESET = '';
      ;

  private static final String SET_CURRENT_PRECISION =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //   EXEC SQL SET CURRENT PRECISION = 'DEC15';
      ;

  private static final String SET_CURRENT_PRECISION2 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //  SET CURRENT QUERY ACCELERATION NONE;
      ;

  private static final String SET_CURRENT_QUERY_ACCELERATION =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //  SET CURRENT QUERY ACCELERATION NONE;
      ;

  private static final String SET_CURRENT_QUERY_ACCELERATION_WAITFORDATA =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // SET CURRENT QUERY ACCELERATION WAITFORDATA = 180.0;
      ;

  private static final String SET_CURRENT_QUERY_ACCELERATION_WAITFORDATA2 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // SET CURRENT QUERY ACCELERATION WAITFORDATA = 2.5;
      ;

  private static final String SET_CURRENT_REFRESH_AGE =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //  SET CURRENT REFRESH AGE ANY;
      ;

  private static final String SET_CURRENT_ROUTINE_VERSION =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //    SET CURRENT ROUTINE VERSION = :rvid;
      ;

  private static final String SET_CURRENT_RULES =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //  EXEC SQL SET CURRENT RULES = 'DB2';
      ;

  private static final String SET_CURRENT_SQLID =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //   SET CURRENT SQLID = SESSION_USER;
      ;

  private static final String SET_CURRENT_TEMPORAL_BUSINESS_TIME =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // SET CURRENT TEMPORAL BUSINESS_TIME = TIMESTAMP('2008-01-01') + 5 DAYS ;
      // SET CURRENT TEMPORAL BUSINESS_TIME = '2008-01-06-00.00.00.000000000000';
      ;

  private static final String SET_CURRENT_TEMPORAL_SYSTEM_TIME =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // SET CURRENT TEMPORAL SYSTEM_TIME = TIMESTAMP('2008-01-01') + 5 DAYS;
      // SET CURRENT TEMPORAL SYSTEM_TIME = '2008-01-06-00.00.00.000000000000';
      ;

  private static final String SET_CURRENT_TEMPORAL_SYSTEM_TIME2 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // SET CURRENT TEMPORAL SYSTEM_TIME = NULL
      ;

  private static final String SET_ENCRYPTION_PASSWORD =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //  SET ENCRYPTION PASSWORD = :hv1
      ;

  private static final String SET_ENCRYPTION_PASSWORD2 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //  SET ENCRYPTION PASSWORD = :hv1 WITH HINT :hv2
      ;

  private static final String SET_PATH =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //    SET PATH = SCHEMA1,"SCHEMA#2", SYSIBM;
      ;

  private static final String SET_PATH2 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //   SET PATH = CURRENT PATH, SMITH, SYSPROC;
      ;

  private static final String SET_SCHEMA =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //   EXEC SQL SET SCHEMA RICK;
      ;

  private static final String SET_SCHEMA2 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //   EXEC SQL SELECT CURRENT SCHEMA INTO :CURSCHEMA
      //      FROM SYSIBM.SYSDUMMY1;
      ;
  private static final String SET_SCHEMA3 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // SET CURRENT SQLID = 'USRT001';
      // SET CURRENT SCHEMA = 'USRT002';
      ;

  private static final String SET_SCHEMA4 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //   SET CURRENT SCHEMA = 'JOHN';
      ;

  private static final String SET_SESSION_TIME_ZONE =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //	SET SESSION TIME ZONE = '-8:00';
      ;

  @ParameterizedTest
  @MethodSource("textsToTest")
  @DisplayName("Parameterized - sql all set statements tests")
  void test(String text) {
    UseCaseEngine.runTest(text, List.of(), Map.of());
  }
}
