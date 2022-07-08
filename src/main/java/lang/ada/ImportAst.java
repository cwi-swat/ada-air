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
        System.loadLibrary("native");
    }
    
    public void importAdaAst(ISourceLocation adaFile, ISourceLocation outFile) {
        try {
            String adaFileName = locToString(URIResolverRegistry.getInstance().logicalToPhysical(adaFile));
            String outFileName = locToString(URIResolverRegistry.getInstance().logicalToPhysical(outFile));
            this._importAdaAst(adaFileName, outFileName);
        } 
        catch (IOException e) {
            throw RuntimeExceptionFactory.io(e.getMessage());
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

    private native void _importAdaAst(String adaFileName, String outFileName);
}
