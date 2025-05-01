{-# LANGUAGE BangPatterns #-}
module Main where

    import System.Directory
    import Control.Monad
    import Control.Applicative

    data Cargo = Estagiario | Programador | Coordenador | Gerente deriving (Read, Show)

    data Pessoa = Pessoa {cargo :: Cargo, nome :: String} deriving (Show, Read)

    data Indice a = Indice {indice :: Int, dados :: a} deriving (Show, Read)

    data Projeto = Projeto {nomeProjeto :: String, budget :: Double, envolvidos :: [Int]} deriving (Read, Show)

    instance Functor Indice where
        fmap f (Indice i dados) = Indice i (f dados)

    (/>) :: a -> (a -> b) -> b 
    (/>) x f = f x 

    infixl 9 />

    class ToJSON a where
        toJSON :: a -> String

    instance ToJSON a => ToJSON (Indice a) where
        toJSON p = "{id: " ++ show (indice p) ++ ", dados: " ++ (toJSON $ dados p) ++ "}"

    instance ToJSON Pessoa where
            toJSON p = "{nome: \"" ++ (nome p) ++ "\", cargo: \"" ++ show (cargo p) ++ "\", salario: " ++ show (verSalario p) ++ "}"

    instance ToJSON Projeto where
        toJSON p = "{nome: \"" ++ (nomeProjeto p) ++ "\", orçamento: \"" ++ show (budget p) ++ "\", envolvidos: " ++ show (envolvidos p) ++ "}"

    instance Monoid Projeto where
            mempty = Projeto "" 0 []
            mappend (Projeto nome1 budget1 env1) (Projeto nome2 budget2 env2) = Projeto (nome1 ++ ", " ++ nome2) (budget1 + budget2) (env1 ++ env2)

            verSalario :: Pessoa -> Double
            verSalario (Pessoa Estagiario _) = 1500
            verSalario (Pessoa Programador _) = 5750.15
            verSalario (Pessoa Coordenador _) = 8000
            verSalario (Pessoa Gerente _) = 10807.20

            verFolha :: Pessoa -> String
            verFolha p = "{nome: \"" ++ (nome p) ++ "\", cargo: \"" ++ show (cargo p) ++ "\", salario: " ++ show (verSalario p) ++ "}"

            contratar :: Cargo -> (String -> Pessoa)
            contratar cargo = Pessoa cargo

            promover :: Pessoa -> Pessoa
            promover (Pessoa Estagiario n) = Pessoa Programador n
            promover (Pessoa Programador n) = Pessoa Coordenador n
            promover (Pessoa Coordenador n) = Pessoa Gerente n
            promover (Pessoa _ n) = Pessoa Gerente n

            mediaSalarial :: [Pessoa] -> Double
            mediaSalarial ps = (foldl calculo 0 ps) / (fromIntegral $ length ps)
                where
                    calculo salario pessoa = salario + verSalario pessoa

            contratarVariosEstag :: [String] -> [Pessoa]        
            contratarVariosEstag ps = map (contratar Estagiario) ps 

            pesquisarPorNome :: String -> [Pessoa] -> [Pessoa]
            pesquisarPorNome nomePesq ps = filter (\(Pessoa _ n) -> nomePesq == n) ps

            rotinaPromocao :: Pessoa -> String 
            rotinaPromocao p = p 
                            |> promover 
                            |> verFolha 

            cadastroPessoa :: IO ()
            cadastroPessoa = do 
                !pessId <- fmap (length . lines) (readFile "func.dat")
                pess <- Pessoa <$> (putStrLn "Cargo: " >> readLn) <*> (putStrLn "Nome: " >> getLine) 
                appendFile "func.dat" (show (Indice (1+pessId) pess) ++ "\n")
                putStrLn "Usuário cadastrado com sucesso!"

            cadastroProjeto :: IO ()
            cadastroProjeto = do 
                !projId <- fmap (length . lines) (readFile "projetos.dat")
                proj <- Projeto <$> (putStrLn "Nome: " >> getLine) <*> (putStrLn "Orçamento: " >> readLn) <*> (putStrLn "Envolvidos: " >> read Ln) 
                appendFile "projetos.dat" (show (Indice (1+projId) proj) ++ "\n")
                putStrLn "Projeto cadastrado com sucesso!"

            buscaPessoas :: IO () 
            buscaPessoas = do 
                putStrLn "Digite o id do funcionário"
                pessId <- readLn 
                pess <- buscaPorId pessId "func.dat" :: IO (Maybe (Indice Pessoa))
                case pess of
                    Nothing -> putStrLn "Erro..." 
                    Just p -> print p 

            buscaProjetos :: IO () 
            buscaProjetos = do 
                putStrLn "Digite o id do projeto" 
                projId <- readLn 
                proj <- buscaPorId projId "projetos.dat" :: IO (Maybe (Indice Projeto)) 
                case proj of  
                    Nothing -> putStrLn "Erro..."
                    Just (Indice i (Projeto nm bd env)) -> do 
                        print $ "id :" ++ show i  print $ "nome :" ++ nm 
                        print $ "orçamento :" ++ show bd 
                        forM_ env $ \envId -> do  
                            pess <- buscaPorId envId "func.dat" :: IO (Maybe Indice Pessoa) 
                            case pess of   
                                Nothing -> print "Pessoa inválida"
                                Just p -> print p  

            buscaPorId :: (Read a, Show a) => Int -> String -> IO (Maybe (Indice a)) 
            buscaPorId tid arq = do  
                todosReg <- fmap lines (readFile arq) 
                todosConv <- return $ map read todosReg  
                res <- return $ filter (\ips -> indice ips == tid) todosConv 
                case res of 
                    [] -> return Nothing
                    _ -> return $ Just (head res)

            todosSalarios :: IO () 
            todosSalarios = do 
                putStrLn "Listando salários..." 
                todasPessoas <- fmap lines (readFile "func.dat") 
                todasPess <- return $ map read todasPessoas :: IO [Indice Pessoa]
                forM_ todasPess $ \(Indice _ pessoa) ->  
                    print $ "Nome : " ++ (nome pessoa) ++ ", Salário: " ++ show (verSalario pessoa)

            exportarPessoas :: IO ()
            exportarPessoas = do 
                writeFile "func.json" ""
                todasPessoas <- fmap lines (readFile "func.dat") 
                todasPess <- return $ map read todasPessoas :: IO [Indice Pessoa]
                mapM_ (\x -> appendFile "func.json" (toJSON x ++ "\n")) todas Pess 

                menu :: IO Int 
                menu :: IO Int
                menu = do
                    putStrLn "Lambda Systemas v1.0"
                    putStrLn "Digite uma opção... " 
                    putStrLn "1- Cadastro de Pessoas" 
                    putStrLn "2- Cadastro de Projetos"
                    putStrLn "3- Busca de Pessoas"
                    putStrLn "4- Busca de Projetos" 
                    putStrLn "5- Total gasto por projeto" 
                    putStrLn "6- Ver salários"
                    putStrLn "7- Exportar pessoas (JSON)"
                    putStrLn "8- Exportar Projetos (JSON)" 
                    opcao <- readLn 
                    if opcao < 0 || opcao > 8 then do 
                        putStrLn "Opção inválida"
                        menu
                    else 
                        return opcao

                main :: IO ()
                main = do 
                    let projetos = "projetos.dat" 
                    pessoas = "func.dat" 
                    existeProj <- doesFileExist projetos 
                    existeFunc <- doesFileExist pessoas 
                    when (not existeProj) (writeFile projetos "") 
                    when (not existeFunc) (writeFile pessoas "")
                    opcao <- menu 
                    case opcao of 
                        1 -> cadastroPessoa   
                        2 -> cadastroProjeto
                        3 -> buscaPessoas 
                        4 -> buscaProjetos 
                        6 -> todosSalarios 
                        7 -> exportarPessoas 