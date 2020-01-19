module MigrateDB where

import Repository.SqliteUtils (migrateDB)

main :: IO ()
main = migrateDB 