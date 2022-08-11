if ($null -eq $env:JAVA_HOME){
	Write-Output "Please set the JAVA_HOME environment variable"
	exit
}
pip install -U git+https://github.com/AdaCore/langkit.git@22.0
$env:PYTHONPATH='.\src\langkit-plugin\' 
python .\Libadalang\manage.py generate --plugin-pass=rascal_plugin.RascalPass
cd .\src\main\ada\; alr build; cd ..\..\..
mvn compile
