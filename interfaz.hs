import Control.Monad (when)


-- agregar producto al inventario
addProduct :: [(String, Double, Int)] -> String -> Double -> Int -> [(String, Double, Int)]
addProduct inventory name price quantity = inventory ++ [(name, price, quantity)]

-- actualizar producto existente
updateQuantity :: [(String, Double, Int)] -> String -> Int -> [(String, Double, Int)]
updateQuantity [] _ _ = []
updateQuantity ((n, p, q):xs) name newQuantity
    | n == name = (n, p, newQuantity) : xs
    | otherwise = (n, p, q) : updateQuantity xs name newQuantity

-- eliminar un producto del inventario
removeProduct :: [(String, Double, Int)] -> String -> [(String, Double, Int)]
removeProduct inventory name = filter (\(n, _, _) -> n /= name) inventory

-- obtener el resumen del inventario: total de productos y valor total
inventorySummary :: [(String, Double, Int)] -> (Int, Double)
inventorySummary inventory = (totalQuantity, totalValue)
  where
    totalQuantity = sum [q | (_, _, q) <- inventory]
    totalValue = sum [p * fromIntegral q | (_, p, q) <- inventory]

-- buscar un producto por su nombre
findProduct :: [(String, Double, Int)] -> String -> Maybe (Double, Int)
findProduct [] _ = Nothing
findProduct ((n, p, q):xs) name
    | n == name = Just (p, q)
    | otherwise = findProduct xs name

-- aplicar un descuento a todos los productos del inventario
applyDiscount :: [(String, Double, Int)] -> Double -> [(String, Double, Int)]
applyDiscount inventory discount = map (\(n, p, q) -> (n, p * (1 - discount), q)) inventory

-- función principal
main :: IO ()
main = do
    putStrLn "Sistema de Gestión de Inventario"
    loop []
  where
    loop inventory = do
        putStrLn "\nSeleccione una opción:"
        putStrLn "1. Agregar producto"
        putStrLn "2. Actualizar cantidad"
        putStrLn "3. Eliminar producto"
        putStrLn "4. Ver resumen del inventario"
        putStrLn "5. Buscar producto"
        putStrLn "6. Aplicar descuento"
        putStrLn "7. Salir"
        putStr "Opción: "
        option <- getLine
        case option of
            "1" -> do
                putStr "Nombre del producto: "
                name <- getLine
                putStr "Precio: "
                price <- readLn
                putStr "Cantidad: "
                quantity <- readLn
                loop (addProduct inventory name price quantity)
            "2" -> do
                putStr "Nombre del producto a actualizar: "
                name <- getLine
                putStr "Nueva cantidad: "
                quantity <- readLn
                loop (updateQuantity inventory name quantity)
            "3" -> do
                putStr "Nombre del producto a eliminar: "
                name <- getLine
                loop (removeProduct inventory name)
            "4" -> do
                let (totalQty, totalValue) = inventorySummary inventory
                putStrLn $ "Total de productos: " ++ show totalQty
                putStrLn $ "Valor total del inventario: " ++ show totalValue
                loop inventory
            "5" -> do
                putStr "Nombre del producto a buscar: "
                name <- getLine
                case findProduct inventory name of
                    Just (price, quantity) -> putStrLn $ "Producto encontrado: " ++ name ++ ", Precio: " ++ show price ++ ", Cantidad: " ++ show quantity
                    Nothing -> putStrLn "Producto no encontrado"
                loop inventory
            "6" -> do
                putStr "Descuento a aplicar (porcentaje, ej: 0.1 para 10%): "
                discount <- readLn
                loop (applyDiscount inventory discount)
            "7" -> putStrLn "Saliendo del sistema..."
            _   -> do
                putStrLn "Opción inválida, intente nuevamente."
                loop inventory
