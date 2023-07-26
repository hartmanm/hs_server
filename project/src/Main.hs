{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as LBCS
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe,mapMaybe)
import Data.Text (pack,strip,unpack)
import Data.Text.Encoding (decodeUtf8)
import Network.HTTP.Types (status200)
import Network.HTTP.Types.Header (hContentType)
import Network.Wai (Request,rawPathInfo,rawQueryString,requestMethod,responseLBS)
import Network.Wai.Handler.Warp (run)
import System.Directory (canonicalizePath,setCurrentDirectory,getCurrentDirectory,listDirectory,doesDirectoryExist,createDirectory,removeDirectory,doesFileExist)
import System.FilePath ((</>),addTrailingPathSeparator,takeFileName,makeRelative,takeExtension)
import Text.PrettyPrint (render)
import qualified Text.PrettyPrint as PP
import System.IO (readFile)
import Data.List (isPrefixOf)
import Control.Monad (foldM)
import System.FilePath.Find (find,always,fileType,FileType(RegularFile),(==?))
import Data.List.Split (splitOn)
import Data.Bool (bool)

walk_path_recursively :: FilePath -> IO ([(FilePath,[FilePath])],[FilePath])
walk_path_recursively path = do
    paths <- find always (fileType ==? RegularFile) path
    let directories_and_files = [(path, map (makeRelative path) paths)]
    return (directories_and_files, paths)

get_fullpath_from_relative_path :: String -> IO FilePath
get_fullpath_from_relative_path file_name = do
    pwd <- getCurrentDirectory
    return $ pwd </> file_name

generate_map_from_routes_html_data :: FilePath -> IO (Map.Map String String)
generate_map_from_routes_html_data filePath = do
    routes_content <- readFile filePath
    let routes_content_as_list = mapMaybe h_parse_line $ lines routes_content
    return $ Map.fromList routes_content_as_list

h_parse_line :: String -> Maybe (String, String)
h_parse_line line = case splitOn " " line of
    (k:v:_) -> Just (unpack $ strip $ pack k, unwords $ drop 1 $ words line)
    _ -> Nothing

tr_substring :: String -> String -> String -> String
tr_substring _ _ [] = []
tr_substring find replacement str@(x:xs)
    | find `isPrefixOf` str = replacement ++ tr_substring find replacement (drop (length find) str)
    | otherwise = x : tr_substring find replacement xs

get_mounted_volume_name :: Map.Map String String -> String
get_mounted_volume_name html_content_map = case Map.lookup "/mounted_volume" html_content_map of
    Just value -> value
    Nothing    -> ""

-- only modifying the internal mountpoint to the host path
pp_html_content_map :: Map.Map String String -> PP.Doc
pp_html_content_map html_content_map = do
    let mounted_volume_name = get_mounted_volume_name html_content_map
    PP.vcat $ map (h_key_value mounted_volume_name) $ Map.toList html_content_map
    where
      h_key_value :: String -> (String, String) -> PP.Doc
      h_key_value mount_name (k, v) = PP.text "<a href='" PP.<+> PP.text k PP.<+> PP.text "'>" PP.<+> PP.text (tr_substring "/hd" mount_name k) PP.<+> PP.text "</a><br><br>"

get_value_or_render :: B.ByteString -> Map.Map String String -> String
get_value_or_render path html_content_map = case Map.lookup (unpack $ decodeUtf8 path) html_content_map of
  Just html -> h_strip_leading_forward_slash html
  Nothing -> "choose from the available routes:<br><br>" ++ render (pp_html_content_map html_content_map) 

h_strip_leading_forward_slash :: String -> String
h_strip_leading_forward_slash ('/' : xs) = xs
h_strip_leading_forward_slash xs = xs

get_port :: Map.Map String String -> Int
get_port html_content_map = read (get_value_or_render "/serve_port" html_content_map) :: Int

get_target_directory :: Map.Map String String -> String
get_target_directory html_content_map = read (get_value_or_render "/target_directory" html_content_map) :: [Char]

add_mounted_directories_and_files_to_html_content_map :: Map.Map String String -> [(FilePath, [FilePath])] -> IO (Map.Map String String)
add_mounted_directories_and_files_to_html_content_map html_content_map directories_and_files = foldM h_1_add_mounted_directories_and_files_to_html_content_map html_content_map directories_and_files

h_1_add_mounted_directories_and_files_to_html_content_map :: Map.Map String String -> (FilePath, [FilePath]) -> IO (Map.Map String String)
h_1_add_mounted_directories_and_files_to_html_content_map html_content_map (dir, paths) = do
    let absolutepaths = map (dir </>) paths
    foldM h_translate_file_contents_to_html html_content_map absolutepaths

h_translate_file_contents_to_html :: Map.Map String String -> FilePath -> IO (Map.Map String String)
h_translate_file_contents_to_html html_content_map file = do
    let extension = takeExtension file
    if extension `elem` [".jpeg",".jpg",".bin",".pdf"]
        then do
            contents <- B.readFile file
            let binary_content = B.unpack contents
            let file_contents = map show binary_content
            let updated_map = h_add_to_map file (concat file_contents) html_content_map
            putStrLn ""
            return updated_map
        else do
            contents <- h_2_add_mounted_directories_and_files_to_html_content_map file
            case contents of
                Just file_contents -> do
                    let updated_map = h_add_to_map file ("<html> " ++ tr_substring "\n" "<br>" file_contents ++ " </html>") html_content_map
                    putStrLn ""
                    return updated_map
                Nothing -> do
                    putStrLn $ "Skipping non-existent file: " ++ file
                    return html_content_map

h_2_add_mounted_directories_and_files_to_html_content_map :: FilePath -> IO (Maybe String)
h_2_add_mounted_directories_and_files_to_html_content_map file = do
    file_exists <- doesFileExist file
    let file_name = takeFileName file
    if file_exists && not (h_is_hidden_file file_name)
        then readFile file >>= return . Just
        else return Nothing

h_is_hidden_file :: FilePath -> Bool
h_is_hidden_file file_name = "." `isPrefixOf` file_name

h_add_to_map :: Ord k => k -> v -> Map.Map k v -> Map.Map k v
h_add_to_map key value map_to_update = Map.insert key value map_to_update

pp_map_keys :: Map.Map String String -> PP.Doc
pp_map_keys = PP.vcat . map h_key_value . Map.toList
    where
      h_key_value (k, _) = PP.text k PP.<+> PP.text "\n"

main = do
  putStrLn "\nserving\n"
  html_routes_file_path <- get_fullpath_from_relative_path "modified_routes"
  html_content_map <- generate_map_from_routes_html_data html_routes_file_path
  setCurrentDirectory "/hd"
  mounted_directory <- getCurrentDirectory
  (directories_and_files, _) <- walk_path_recursively mounted_directory
  html_content_map <- add_mounted_directories_and_files_to_html_content_map html_content_map directories_and_files
  print (pp_map_keys html_content_map)
  run (get_port html_content_map) $ \req respond -> do
      let path = rawPathInfo req
      let response = case requestMethod req of
            "GET" -> get_value_or_render path html_content_map
            _ -> "Error: unsupported HTTP method!"
      respond $ responseLBS status200 [(hContentType, "text/html")] (LBCS.pack response)
