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

-- Función para aplicar un descuento a todos los productos del inventario
applyDiscount :: [(String, Double, Int)] -> Double -> [(String, Double, Int)]
applyDiscount inventory discount = map (\(n, p, q) -> (n, p * (1 - discount), q)) inventory

-- uso del sistema de gestión de inventario
main :: IO ()
main = do
    -- Inicializar el inventario vacío
    let inventory = []

    -- Agregar productos al inventario
    let inventory1 = addProduct inventory "Manzanas" 0.5 100
    let inventory2 = addProduct inventory1 "Platanos" 0.3 150

    -- Actualizar la cantidad de un producto
    let inventory3 = updateQuantity inventory2 "Manzanas" 120

    -- Eliminar un producto del inventario
    let inventory4 = removeProduct inventory3 "Platanos"

    -- Obtener el resumen del inventario
    let (totalQty, totalValue) = inventorySummary inventory4

    -- Mostrar el inventario final y el resumen
    putStrLn $ "Inventario Final: " ++ show inventory4
    putStrLn $ "Total de productos en stock: " ++ show totalQty
    putStrLn $ "Valor total del inventario: " ++ show totalValue

    -- Buscar un producto en el inventario
    case findProduct inventory4 "Manzanas" of
        Just (price, quantity) -> putStrLn $ "Producto encontrado: Manzanas, Precio: " ++ show price ++ ", Cantidad: " ++ show quantity
        Nothing -> putStrLn "Producto no encontrado"

    -- Aplicar un descuento del 10% a todos los productos
    let inventory5 = applyDiscount inventory4 0.1
    putStrLn $ "Inventario con descuento: " ++ show inventory5
