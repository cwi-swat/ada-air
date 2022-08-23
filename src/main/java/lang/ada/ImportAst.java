/*
Copyright (c) 2022, TNO (ESI) and NWO-I Centrum Wiskunde & Informatica (CWI)
All rights reserved.

Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

1. Redistributions of source code must retain the above copyright
notice, this list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright
notice, this list of conditions and the following disclaimer in the
documentation and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY
WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY
OF SUCH DAMAGE.

Authors:
Jurgen J. Vinju - Centrum Wiskunde & Informatica
Damien De Campos - TNO ESI
Pierre van de Laar - TNO ESI
*/

package lang.ada;
import java.io.File;
import java.io.IOException;

import org.rascalmpl.exceptions.RuntimeExceptionFactory;
import org.rascalmpl.uri.URIResolverRegistry;

import io.usethesource.vallang.ISourceLocation;
import io.usethesource.vallang.IValueFactory;

public class ImportAst {

    private final IValueFactory vf;

	public ImportAst(IValueFactory vf) {
		this.vf = vf;
	}

    static {
        System.loadLibrary("lal_to_rascal");
    }
    
    public void importAdaAst(ISourceLocation adaFile, ISourceLocation outFile) throws AdaException {
        String adaFileName, outFileName;
        try {
            adaFileName = locToString(URIResolverRegistry.getInstance().logicalToPhysical(adaFile));
            outFileName = locToString(URIResolverRegistry.getInstance().logicalToPhysical(outFile));
        } 
        catch (IOException e) {
            throw RuntimeExceptionFactory.io(e.getMessage());
        }
        try {
            this._importAdaAst(adaFileName, outFileName);
        }
        catch (AdaException e) {
            throw RuntimeExceptionFactory.javaException(e, null, null);
        }
    }

    public void importAdaProject(ISourceLocation adaProjectFile, ISourceLocation outFile) throws AdaException {
        String adaFileName, outFileName;
        try {
            adaFileName = locToString(URIResolverRegistry.getInstance().logicalToPhysical(adaProjectFile));
            outFileName = locToString(URIResolverRegistry.getInstance().logicalToPhysical(outFile));
        } 
        catch (IOException e) {
            throw RuntimeExceptionFactory.io(e.getMessage());
        }
        try {
            this._importAdaProject(adaFileName, outFileName);
        }
        catch (AdaException e) {
            throw RuntimeExceptionFactory.javaException(e, null, null);
        }
    }

    private String locToString(ISourceLocation l) {
        if (!"file".equals(l.getScheme())) {
            throw RuntimeExceptionFactory.illegalArgument(l,
                "only file:/// URI are supported or logical schemes that derived from file");
        }
        else {
            return vf.string(new File(l.getURI()).getAbsolutePath()).getValue();
        }
    }

    private native void _importAdaAst(String adaFileName, String outFileName) throws AdaException;

    private native void _importAdaProject(String adaFileName, String outFileName) throws AdaException;
}
