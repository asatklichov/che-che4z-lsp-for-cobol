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
 */

package com.broadcom.lsp.domain.cobol.event.factory;

import com.broadcom.lsp.domain.cobol.event.api.CopybookSubscriber;
import com.broadcom.lsp.domain.cobol.event.api.EventObserver;
import com.broadcom.lsp.domain.cobol.event.impl.AnalysisFinishedEventSubscriber;
import com.broadcom.lsp.domain.cobol.event.model.AnalysisFinishedEvent;

public class AnalysisFinishedSubscriberFactory
    implements CopybookSubscriber<AnalysisFinishedEventSubscriber> {

    @Override
    public AnalysisFinishedEventSubscriber create(EventObserver observer) {
        return new AnalysisFinishedEventSubscriber(observer, new AnalysisFinishedEvent());
    }
}
