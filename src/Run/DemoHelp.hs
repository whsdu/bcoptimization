module Run.DemoHelp where 


import Control.Monad.IO.Class (MonadIO) 
import Control.Monad.Reader (MonadReader)

import Run.Env ( grab, runApp,App)
import ASPIC.Abstraction (Has)

import qualified ASPIC.Defeasible as D 

import qualified Parser.FileParser as FP



getRunner :: FilePath -> App () b -> IO b
getRunner filePath func = do 
    env <- FP.parseDefaultEnv filePath 
    runApp env func 

bugRunner :: FilePath -> IO (App () b-> IO b)
bugRunner filePath = do 
    env <- FP.parseDefaultEnv filePath 
    pure $ runApp env 

getProposition :: FilePath -> String  -> IO(D.Literal ())
getProposition filePath pro = do 
    env <- FP.parseDefaultEnv filePath
    let 
        runner = runApp env
        lMap = FP.parseLiteralMap env
    pure $ FP.parseQueryLiteral pro lMap