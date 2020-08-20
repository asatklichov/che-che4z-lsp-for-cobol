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

package com.ca.lsp.core.cobol.preprocessor.sub.document;

import com.ca.lsp.core.cobol.model.CopybookModel;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/** This interface represents the API for retrieving copybooks */
public interface CopybookResolution {

  /**
   * Retrieve and return the copybook by its name.
   *
   * @param copybookName - the name of the copybook to be retrieved
   * @param documentUri - the currently processing document that contains the copy statement
   * @param syncType - text document synchronization type
   * @return a CopybookModel that contains copybook name, its URI and the content
   */
  @Nullable
  CopybookModel resolve(
      @Nonnull String copybookName, @Nonnull String documentUri, @Nonnull String syncType);
}
