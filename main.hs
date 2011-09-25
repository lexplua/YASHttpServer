import Network (listenOn , withSocketsDo, accept, PortID(..), Socket)
import System ( getArgs)
import System.IO 
import Control.Concurrent(forkIO)
import Control.Exception
{-----config------}

dataDir = "./htdocs"
port = 8080
contentType = "text/html"
encoding = "uft-8"
{---end config----}

sockHandler::Socket->IO()
sockHandler sock = do
	( handle , _ , _ ) <- accept sock
	hSetBuffering handle NoBuffering
	forkIO $ requestProcessor handle
	sockHandler sock

requestProcessor::Handle -> IO()
requestProcessor handle = do
	line <- hGetLine handle
	doRequest handle line


doRequest::Handle -> String -> IO()
doRequest handle string = do
	putStrLn string
	if (head $ words string) == "GET" 
		then do
			hPutStrLn handle ("HTTP/1.1 200 OK\r\nContent-type:"++ contentType ++"; charset=" ++ encoding ++";\r\n")
			hDataFile <-try( openFile (dataDir ++ ((words string)!!1)) ReadMode ){-(\_-> hPutStrLn handle "No such file or directory")-}
			contents <- hGetContents hDataFile
			hPutStrLn handle contents
			hClose hDataFile
			

		else do
			return()
	
main::IO()
main = withSocketsDo $ do
		let sPort = fromIntegral port
		sock <- listenOn $ PortNumber sPort
		putStrLn $ "Listening on " ++ (show port)
		sockHandler sock

