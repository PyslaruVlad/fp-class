{-
  Для тестирования программ, работающих с файловой системой, часто необходимо
  достаточно большое дерево каталогов с файлами. Реализуйте случайный генератор
  такого дерева, управляемый набором параметров (минимальная и максимальная ширина
  и глубина дерева, количество и размеры файлов, что-нибудь ещё). В качестве идеи
  для архитектуры приложения используйте пример с подсчётом количества файлов в
  дереве (count.hs). Этот же пример можно использовать для тестирования
  разработанного приложения.
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import System.Environment
import System.Random
import System.Directory
import System.FilePath
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.State

type ContentGenParams = ((Int, Int), (Char, Char))

data GenConfig = GenConfig {
        fileNameParams::ContentGenParams,
        contentParams::ContentGenParams, -- не совсем, конечно, размер, но надеюсь сойдет.
        dirNameParams::ContentGenParams,
        filesQRange::(Int, Int),
        dirsQRange::(Int, Int),
        maxDepth::Int
    }

data GenState = GenState {
        curDepth::Int,
        curPath::FilePath
    }

generateQuantity :: (Int, Int) -> ReaderT GenConfig (StateT GenState IO) Int
generateQuantity rng = do
    gen <- liftIO $ newStdGen
    let flsQ = fst $ randomR rng gen
    return flsQ

generateContent :: (GenConfig -> ContentGenParams) -> 
    ReaderT GenConfig (StateT GenState IO) String
generateContent func = do
    gen <- liftIO $ newStdGen
    (rng, alph)  <- func `liftM` ask
    q <- generateQuantity rng
    return $ take q $ randomRs alph gen

-- Повторяет цикл генерации случайного файла, пока не получится создать файл с
-- уникальным именем.
generateFile :: ReaderT GenConfig (StateT GenState IO) ()
generateFile = do
    path <- lift $ curPath `liftM` get
    newName <- generateContent fileNameParams
    content <- generateContent contentParams
    let fullName = path </> newName
    exists <- liftIO $ doesFileExist fullName
    if exists
    then generateFile
    else liftIO $ writeFile fullName content

generateDirectory :: ReaderT GenConfig (StateT GenState IO) ()
generateDirectory = do
    old@(GenState depth path) <- lift get
    newName <- generateContent dirNameParams
    let fullName = path </> newName
    exists <- liftIO $ doesDirectoryExist fullName
    if exists
    then generateDirectory
    else do 
        liftIO $ createDirectory fullName
        put (GenState (depth+1) fullName)
        generator
        put old

generator :: ReaderT GenConfig (StateT GenState IO) ()
generator = do
    maxDepth <- maxDepth `liftM` ask
    curDepth <- lift $ curDepth `liftM` get
    when (curDepth <= maxDepth) $ do
        flsRng <- filesQRange `liftM` ask
        fq <- generateQuantity flsRng
        replicateM fq generateFile
        when (curDepth < maxDepth) $ do
            drsRng <- dirsQRange `liftM` ask
            dq <- generateQuantity drsRng
            replicateM dq generateDirectory
            return ()
        
runGenerator :: GenConfig -> FilePath -> IO ()
runGenerator cnfg initDir = evalStateT (runReaderT generator cnfg) initState
    where initState = GenState 0 initDir




-- Уж очень не хочется вводить это все в консоли.

defFileName = ((3, 5), ('A','z'))
defDirName = ((3, 5), ('A','z'))
defContent = ((15, 30), (' ','~'))
defaultConfig = GenConfig defFileName defContent defDirName (4, 4) (3, 3) 2

-- Протестировать программу можно непосредственно.

main = do
    let initDir = "./randomRoot"
    exists <- doesDirectoryExist initDir
    if not exists
    then createDirectory initDir
    else return ()
    runGenerator defaultConfig initDir
