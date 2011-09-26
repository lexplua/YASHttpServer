>  import Network (listenOn , withSocketsDo, accept, PortID(..), Socket)
>  import System ( getArgs)
>  import System.IO 
>  import Control.Concurrent(forkIO)
>  import Control.Exception

start config

>  dataDir = "./htdocs"
>  port = 8080
>  contentType = "text/html"
>  encoding = "uft-8"

end config

> 
>  sockHandler::Socket->IO()
>  sockHandler sock = do
>     ( hSocket , _ , _ ) <- accept sock
>     hSetBuffering hSocket NoBuffering
>     forkIO $ requestProcessor hSocket
>     sockHandler sock
> 
> 
> 
> 
>  requestProcessor::Handle -> IO()
>  requestProcessor hSocket = do
>     line <- hGetLine hSocket
>     doResponse hSocket line
> 
> 
>  doResponse::Handle -> String -> IO()
>  doResponse hSocket requestString = do
>     putStrLn requestString
>     case method of
>         "GET"-> do
>             hPutStrLn hSocket ("HTTP/1.1 200 OK\r\nContent-type:"++ contentType ++"; charset=" ++ encoding ++";\r\n")
>             hDataFile <- openFile (dataDir ++ ((words requestString)!!1)) ReadMode 
>             contents <- hGetContents hDataFile
>             hPutStrLn hSocket contents
>             hClose hDataFile
>         _-> do
>             return()
>         where method = (head $ words requestString) 
> 
> 
> 
>  runServer::Int->IO()
>  runServer port = do
>         let sPort = fromIntegral port
>         sock <- listenOn $ PortNumber sPort
>         putStrLn $ "Listening on " ++ (show port)
>         sockHandler sock
> 
> 
>  main::IO()
>  main = withSocketsDo $ do
>         runServer port
