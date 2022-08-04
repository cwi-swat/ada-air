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
