import Parser
import SimpleParsers
import Control.Applicative hiding (many, optional)
import Control.Monad
import ParseNumbers (natural)
import Control.Monad.Writer

{-
   Определите тип данных, представляющий адрес URL следующего вида:

     <схема>://<логин>:<пароль>@<хост>:<порт>/<URL‐путь>?<параметры>#<якорь>

   Реализуйте соответствующий парсер, считая, что обязательными компонентами
   URL являются только схема с символами "://" и имя хоста, все остальные
   компоненты могут отсутствовать.
-}


data Scheme = FTP | HTTP | HTTPS | Unk String
              deriving (Show, Eq) -- Eq для тестирования.
type Server = String
type Path = String
data URL = URL Scheme Server Path
           deriving Show

scheme = (string "https" >> return HTTPS) <|>
         (string "http" >> return HTTP) <|>
         (string "ftp" >> return FTP) <|>
         Unk `liftM` lowers

url = URL <$>
      scheme <*>
      (string "://" >> many1 (sat (/='/'))) <*>
      many (sat $ const True)



-- Решение.

data Item = UrlScheme Scheme
  | Login String | Password String
  | Host String | Port Int
  | FPath String
  | Query String
  | Anchor String
  deriving (Show, Eq)

data ParserTree =
  PTNode {
    start::String,
    parser::Parser Item,
    nodes::[ParserTree]
  } |
  PTLeaf

evalTree :: ParserTree -> WriterT [Item] Parser ()
evalTree PTLeaf = return ()
evalTree node = do
  lift $ string $ start node
  res <- lift $ parser $ node
  tell [res]
  msum $ map evalTree $ nodes node

takeUntil :: [Char] -> Parser String
takeUntil endS = many $ sat $ \ a -> and $ map (/=a) endS

readURL :: String -> [Item]
readURL = parse $ execWriterT $ evalTree mainTree


-- Не смог придумать как обобщить дерево.

-- Тип для функций - Parser Item.
portParser = Port <$> natural
pswdParser = Password <$> takeUntil "@"
hostParser = Host <$> takeUntil ":/"
lognParser = Login <$> do
  a <- takeUntil ":@"
  msum $ map char ":@"
  return a

-- Тип для функций - ParserTree.
mainTree = PTNode "" (UrlScheme <$> scheme) [lognTree, hostTree "://"]
lognTree = PTNode "://" lognParser [pswdTree "", hostTree ""]
pswdTree i = PTNode i pswdParser [hostTree "@"]
hostTree i = PTNode i hostParser [portTree, tailTree, PTLeaf]
portTree = PTNode ":" portParser [tailTree, PTLeaf]


-- Тип для функций - Parser Item.
pathParser = FPath <$> takeUntil "#?"
queryParser = Query <$> takeUntil "#"
anchorParser = Anchor <$> takeUntil "?"

-- Тип для функций - ParserTree.
tailTree = PTNode "/" pathParser [queryTree, anchorTree, PTLeaf]
queryTree = PTNode "?" queryParser [anchorTree, PTLeaf]
anchorTree = PTNode "#" anchorParser [PTLeaf]




-- Тесты.

test = readURL `map` testingSetPos == testingAnswers

testingSetPos =
  [ "xxx://site/path?query"
  , "http://usr:pas@site"
  , "x://usr@site:65535"
  , "ftp://site/path#anch"]

testingAnswers =
  [[UrlScheme (Unk "xxx"), Host "site", FPath "path", Query "query"]
  ,[UrlScheme HTTP,Login "usr",Password "pas",Host "site"]
  ,[UrlScheme (Unk "x"),Login "usr",Host "site",Port 65535]
  ,[UrlScheme FTP,Host "site",FPath "path",Anchor "anch"]]
