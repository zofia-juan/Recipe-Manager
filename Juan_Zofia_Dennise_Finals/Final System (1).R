library(shiny) 
library(shinyWidgets) 
library(DBI) 
library(RMySQL) 
library(DT) 
library(base64enc) 
library(digest)  # For password hashing 

# ------------------------- 
# MySQL connection function 
# ------------------------- 
create_connection <- function() { 
  tryCatch({ 
    conn <- dbConnect( 
      RMySQL::MySQL(),         
      dbname = "recipe_db",   
      host = "localhost", 
      port = 3306, 
      user = "root", 
      password = ""          
    ) 
    return(conn) 
  }, error = function(e) { 
    showNotification( 
      paste("Failed to connect to MySQL:", e$message), 
      type = "error", 
      duration = 5 
    ) 
    return(NULL) 
  }) 
} 

init_db <- function() { 
  conn <- create_connection() 
  if (is.null(conn)) return(FALSE) 
  on.exit(dbDisconnect(conn)) 
  
  tryCatch({ 
    # Create images directory if it doesn't exist 
    if (!dir.exists("www")) { 
      dir.create("www") 
    } 
    if (!dir.exists("www/recipe_images")) { 
      dir.create("www/recipe_images") 
    } 
    
    # Create users table if it doesn't exist 
    dbExecute(conn, " 
      CREATE TABLE IF NOT EXISTS users ( 
        id INT AUTO_INCREMENT PRIMARY KEY, 
        username VARCHAR(50) UNIQUE NOT NULL, 
        email VARCHAR(100) UNIQUE NOT NULL, 
        password_hash VARCHAR(255) NOT NULL, 
        full_name VARCHAR(100), 
        created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP, 
        last_login TIMESTAMP NULL, 
        INDEX idx_username (username), 
        INDEX idx_email (email) 
      ) 
    ") 
    
    # Check if recipes table has user_id column, if not add it 
    tables <- dbListTables(conn) 
    if ("recipes" %in% tables) { 
      columns <- dbGetQuery(conn, "SHOW COLUMNS FROM recipes") 
      if (!"user_id" %in% columns$Field) { 
        dbExecute(conn, "ALTER TABLE recipes ADD COLUMN user_id INT DEFAULT 1") 
        dbExecute(conn, "ALTER TABLE recipes ADD FOREIGN KEY (user_id) REFERENCES users(id) ON DELETE CASCADE") 
      } 
    } 
    
    TRUE 
  }, error = function(e) { 
    showNotification( 
      paste("Failed to initialize database:", e$message), 
      type = "error", 
      duration = 5 
    ) 
    FALSE 
  }) 
} 

# ------------------------- 
# Authentication functions 
# ------------------------- 
hash_password <- function(password) { 
  digest(password, algo = "sha256", serialize = FALSE) 
} 

create_user <- function(username, email, password, full_name = "") { 
  conn <- create_connection() 
  if (is.null(conn)) return(list(success = FALSE, message = "Database connection failed")) 
  on.exit(dbDisconnect(conn)) 
  
  tryCatch({ 
    # Check if username or email already exists 
    check_query <- paste0( 
      "SELECT COUNT(*) as count FROM users WHERE username = ", 
      dbQuoteString(conn, username), 
      " OR email = ", 
      dbQuoteString(conn, email) 
    ) 
    result <- dbGetQuery(conn, check_query) 
    
    if (result$count > 0) { 
      return(list(success = FALSE, message = "Username or email already exists")) 
    } 
    
    # Hash password and insert user 
    password_hash <- hash_password(password) 
    insert_query <- paste0( 
      "INSERT INTO users (username, email, password_hash, full_name) VALUES (", 
      dbQuoteString(conn, username), ",", 
      dbQuoteString(conn, email), ",", 
      dbQuoteString(conn, password_hash), ",", 
      dbQuoteString(conn, full_name), ")" 
    ) 
    
    dbExecute(conn, insert_query) 
    return(list(success = TRUE, message = "User created successfully")) 
  }, error = function(e) { 
    return(list(success = FALSE, message = paste("Error:", e$message))) 
  }) 
} 

authenticate_user <- function(username, password) { 
  conn <- create_connection() 
  if (is.null(conn)) return(NULL) 
  on.exit(dbDisconnect(conn)) 
  
  tryCatch({ 
    password_hash <- hash_password(password) 
    query <- paste0( 
      "SELECT id, username, email, full_name FROM users WHERE username = ", 
      dbQuoteString(conn, username), 
      " AND password_hash = ", 
      dbQuoteString(conn, password_hash) 
    ) 
    
    result <- dbGetQuery(conn, query) 
    
    if (nrow(result) == 1) { 
      # Update last login 
      update_query <- paste0( 
        "UPDATE users SET last_login = NOW() WHERE id = ", 
        result$id 
      ) 
      dbExecute(conn, update_query) 
      
      return(result) 
    } 
    
    return(NULL) 
  }, error = function(e) { 
    return(NULL) 
  }) 
} 

# ------------------------- 
# Helper DB functions (updated for user_id) 
# ------------------------- 
load_recipes <- function(user_id, filter = NULL) { 
  conn <- create_connection() 
  if (is.null(conn)) return(data.frame()) 
  on.exit(dbDisconnect(conn)) 
  
  base_query <- paste0("SELECT * FROM recipes WHERE user_id = ", as.integer(user_id)) 
  
  if (is.null(filter) || trimws(filter) == "") { 
    q <- paste0(base_query, " ORDER BY created_at DESC;") 
  } else { 
    f <- paste0("%", gsub("%", "\\%", filter, fixed = TRUE), "%") 
    f_quoted <- dbQuoteString(conn, f) 
    q <- paste0( 
      base_query, 
      " AND (name LIKE ", f_quoted, 
      " OR tags LIKE ", f_quoted, 
      " OR ingredients LIKE ", f_quoted, ")", 
      " ORDER BY created_at DESC;" 
    ) 
  } 
  
  tryCatch({ 
    df <- dbGetQuery(conn, q) 
    return(df) 
  }, error = function(e) { 
    showNotification( 
      paste("Failed to load recipes:", e$message), 
      type = "error" 
    ) 
    return(data.frame()) 
  }) 
} 

save_recipe_image <- function(file_path, recipe_name) { 
  if (is.null(file_path) || file_path == "" || !file.exists(file_path)) { 
    return("") 
  } 
  
  tryCatch({ 
    safe_name <- gsub("[^a-zA-Z0-9]", "_", recipe_name) 
    ext <- tolower(tools::file_ext(file_path)) 
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S") 
    new_filename <- paste0(safe_name, "_", timestamp, ".", ext) 
    new_path <- file.path("www/recipe_images", new_filename) 
    
    file.copy(file_path, new_path, overwrite = TRUE) 
    
    return(new_filename) 
  }, error = function(e) { 
    showNotification( 
      paste("Failed to save image:", e$message), 
      type = "error" 
    ) 
    return("") 
  }) 
} 

insert_recipe <- function(rec, user_id) { 
  conn <- create_connection() 
  if (is.null(conn)) return(FALSE) 
  on.exit(dbDisconnect(conn)) 
  
  q <- paste0( 
    "INSERT INTO recipes (name, category, difficulty, prep_time, cook_time, ingredients, instructions, tags, image_path, user_id) VALUES (",
    dbQuoteString(conn, rec$name), ",", 
    dbQuoteString(conn, rec$category), ",", 
    dbQuoteString(conn, rec$difficulty), ",", 
    as.integer(rec$prep_time), ",", 
    as.integer(rec$cook_time), ",", 
    dbQuoteString(conn, rec$ingredients), ",", 
    dbQuoteString(conn, rec$instructions), ",", 
    dbQuoteString(conn, rec$tags), ",", 
    dbQuoteString(conn, rec$image_path), ",", 
    as.integer(user_id), ");" 
  ) 
  
  tryCatch({ 
    dbExecute(conn, q) 
    TRUE 
  }, error = function(e) { 
    showNotification( 
      paste("Insert failed:", e$message), 
      type = "error" 
    ) 
    FALSE 
  }) 
} 

update_recipe_db <- function(id, rec) { 
  conn <- create_connection() 
  if (is.null(conn)) return(FALSE) 
  on.exit(dbDisconnect(conn)) 
  
  q <- paste0( 
    "UPDATE recipes SET ", 
    "name = ", dbQuoteString(conn, rec$name), ",", 
    "category = ", dbQuoteString(conn, rec$category), ",", 
    "difficulty = ", dbQuoteString(conn, rec$difficulty), ",", 
    "prep_time = ", as.integer(rec$prep_time), ",", 
    "cook_time = ", as.integer(rec$cook_time), ",", 
    "ingredients = ", dbQuoteString(conn, rec$ingredients), ",", 
    "instructions = ", dbQuoteString(conn, rec$instructions), ",", 
    "tags = ", dbQuoteString(conn, rec$tags), ",", 
    "image_path = ", dbQuoteString(conn, rec$image_path), 
    " WHERE id = ", as.integer(id), ";" 
  ) 
  
  tryCatch({ 
    dbExecute(conn, q) 
    TRUE 
  }, error = function(e) { 
    showNotification( 
      paste("Update failed:", e$message), 
      type = "error" 
    ) 
    FALSE 
  }) 
} 

delete_recipe_db <- function(id) { 
  conn <- create_connection() 
  if (is.null(conn)) return(FALSE) 
  on.exit(dbDisconnect(conn)) 
  
  q <- paste0("DELETE FROM recipes WHERE id = ", as.integer(id), ";") 
  
  tryCatch({ 
    dbExecute(conn, q) 
    TRUE 
  }, error = function(e) { 
    showNotification( 
      paste("Delete failed:", e$message), 
      type = "error" 
    ) 
    FALSE 
  }) 
} 

create_image_html <- function(image_path, size = "60px") { 
  if (is.na(image_path) || image_path == "") { 
    return(paste0('<div style="width:', size, ';height:', size, ';background:#1e2739;border-radius:8px;display:flex;align-items:center;justify-content:center;color:#94a3b8;font-size:20px;">🍽️</div>')) 
  } 
  
  full_path <- file.path("www/recipe_images", image_path) 
  if (!file.exists(full_path)) { 
    return(paste0('<div style="width:', size, ';height:', size, ';background:#1e2739;border-radius:8px;display:flex;align-items:center;justify-content:center;color:#94a3b8;font-size:20px;">🍽️</div>')) 
  } 
  
  web_path <- paste0("recipe_images/", image_path) 
  return(paste0('<img src="', web_path, '" style="width:', size, ';height:', size, ';object-fit:cover;border-radius:8px;border:2px solid #1e2739;"/>')) 
} 

find_meal_combinations <- function(user_id, target_minutes, tolerance = 10, combo_filter = "all", difficulty_filter = "any") { 
  conn <- create_connection() 
  if (is.null(conn)) return(list()) 
  on.exit(dbDisconnect(conn)) 
  
  tryCatch({ 
    base_query <- paste0( 
      "SELECT id, name, category, difficulty, prep_time, cook_time, ingredients, instructions, tags, image_path, ", 
      "(prep_time + cook_time) as total_time FROM recipes WHERE user_id = ", as.integer(user_id) 
    ) 
    
    if (difficulty_filter != "any") { 
      base_query <- paste0(base_query, " AND difficulty = ", dbQuoteString(conn, difficulty_filter)) 
    } 
    
    query <- paste0(base_query, " ORDER BY total_time;") 
    df <- dbGetQuery(conn, query) 
    
    if (nrow(df) == 0) return(list()) 
    
    results <- list() 
    
    # Single recipes 
    if (grepl("^single_", combo_filter)) { 
      category <- sub("^single_", "", combo_filter) 
      single_matches <- df[df$category == category &  
                             df$total_time >= (target_minutes - tolerance) &  
                             df$total_time <= (target_minutes + tolerance), ] 
      if (nrow(single_matches) > 0) { 
        for (i in 1:nrow(single_matches)) { 
          results[[length(results) + 1]] <- list( 
            recipes = list(single_matches[i, ]), 
            total_time = single_matches[i, "total_time"], 
            combination_size = 1, 
            combo_type = paste("Single:", category) 
          ) 
        } 
      } 
    } 
    
    # Two-recipe combinations 
    if (grepl("\\+", combo_filter) && length(strsplit(combo_filter, "\\+")[[1]]) == 2) { 
      required_cats <- strsplit(combo_filter, "\\+")[[1]] 
      
      for (i in 1:nrow(df)) { 
        if (df[i, "category"] != required_cats[1]) next 
        for (j in 1:nrow(df)) { 
          if (i == j) next 
          if (df[j, "category"] != required_cats[2]) next 
          
          combo_time <- df[i, "total_time"] + df[j, "total_time"] 
          if (combo_time >= (target_minutes - tolerance) && combo_time <= (target_minutes + tolerance)) { 
            results[[length(results) + 1]] <- list( 
              recipes = list(df[i, ], df[j, ]), 
              total_time = combo_time, 
              combination_size = 2, 
              combo_type = paste("Pairing:", combo_filter) 
            ) 
          } 
        } 
      } 
    } 
    
    # Three-recipe combinations 
    if (grepl("\\+", combo_filter) && length(strsplit(combo_filter, "\\+")[[1]]) == 3) { 
      required_cats <- strsplit(combo_filter, "\\+")[[1]] 
      
      for (i in 1:nrow(df)) { 
        if (df[i, "category"] != required_cats[1]) next 
        for (j in 1:nrow(df)) { 
          if (i == j) next 
          if (df[j, "category"] != required_cats[2]) next 
          for (k in 1:nrow(df)) { 
            if (k == i || k == j) next 
            if (df[k, "category"] != required_cats[3]) next 
            
            combo_time <- df[i, "total_time"] + df[j, "total_time"] + df[k, "total_time"] 
            if (combo_time >= (target_minutes - tolerance) && combo_time <= (target_minutes + tolerance)) { 
              results[[length(results) + 1]] <- list( 
                recipes = list(df[i, ], df[j, ], df[k, ]), 
                total_time = combo_time, 
                combination_size = 3, 
                combo_type = paste("Set:", combo_filter) 
              ) 
            } 
          } 
        } 
      } 
    } 
    
    if (length(results) > 0) { 
      results <- results[order(sapply(results, function(x) abs(x$total_time - target_minutes)))] 
      if (length(results) > 50) { 
        results <- results[1:50] 
      } 
    } 
    
    return(results) 
  }, error = function(e) { 
    showNotification( 
      paste("Failed to find combinations:", e$message), 
      type = "error" 
    ) 
    return(list()) 
  }) 
} 

# ------------------------- 
# UI 
# ------------------------- 
ui <- fluidPage( 
  tags$head( 
    tags$link(href = "https://fonts.googleapis.com/css2?family=Quicksand:wght@300;400;500;600;700&display=swap", rel = "stylesheet"), 
    tags$style(HTML(" 
      * { font-family: 'Quicksand', sans-serif !important; } 
       
      body { 
        background-color: #fefbf3; 
        background-image: repeating-linear-gradient(45deg, transparent, transparent 35px, rgba(255, 255, 255, 0.5) 35px, rgba(255, 255, 255, 0.5) 70px), 
          radial-gradient(circle at 20% 30%, rgba(220, 38, 38, 0.08) 0%, transparent 50%), 
          radial-gradient(circle at 80% 70%, rgba(34, 197, 94, 0.08) 0%, transparent 50%); 
        background-attachment: fixed; 
        min-height: 100vh; 
        padding: 40px 20px; 
      } 
      .container-fluid { 
        position: relative; 
        z-index: 1; 
        max-width: 1400px; 
        margin: 0 auto; 
      } 
      .login-container { 
        max-width: 500px; 
        margin: 100px auto; 
        background: linear-gradient(135deg, #fff5f5 0%, #fffbeb 20%, #fef3c7 40%, #ecfccb 60%, #d1fae5 80%, #fff5f5 100%); 
        background-size: 200% 200%; 
        animation: gradientShift 15s ease infinite; 
        padding: 50px 45px; 
        border-radius: 28px; 
        box-shadow: 0 25px 70px rgba(0, 0, 0, 0.12); 
        border: 4px solid transparent; 
        position: relative; 
      } 
       
      .login-container::before { 
        content: ''; 
        position: absolute; 
        top: -4px; left: -4px; right: -4px; bottom: -4px; 
        background: linear-gradient(135deg, #dc2626 0%, #f97316 20%, #fbbf24 40%, #84cc16 60%, #22c55e 80%, #dc2626 100%); 
        background-size: 200% 200%; 
        animation: gradientShift 15s ease infinite; 
        border-radius: 28px; 
        z-index: -1; 
        opacity: 0.7; 
      } 
       
      @keyframes gradientShift { 
        0%, 100% { background-position: 0% 50%; } 
        50% { background-position: 100% 50%; } 
      } 
       
      .login-title { 
        font-size: 48px; 
        font-weight: 700; 
        background: linear-gradient(135deg, #dc2626 0%, #f97316 25%, #fbbf24 50%, #22c55e 75%, #dc2626 100%); 
        background-size: 200% auto; 
        -webkit-background-clip: text; 
        -webkit-text-fill-color: transparent; 
        margin-bottom: 30px; 
        text-align: center; 
        animation: shimmer 4s linear infinite; 
      } 
       
      @keyframes shimmer { to { background-position: 200% center; } } 
       
      .login-subtitle { 
        font-size: 18px; 
        color: #78350f; 
        margin-bottom: 30px; 
        text-align: center; 
        font-weight: 500; 
      } 
       
       .main-header { 
        background: linear-gradient(135deg,  
          #fff5f5 0%,  
          #fffbeb 20%,  
          #fef3c7 40%,  
          #ecfccb 60%,  
          #d1fae5 80%,  
          #fff5f5 100%); 
        background-size: 200% 200%; 
        animation: gradientShift 15s ease infinite; 
        padding: 50px 45px; 
        margin-bottom: 35px; 
        border-radius: 28px; 
        box-shadow:  
          0 25px 70px rgba(0, 0, 0, 0.12), 
          0 8px 20px rgba(0, 0, 0, 0.06), 
          inset 0 2px 0 rgba(255, 255, 255, 0.8), 
          inset 0 -2px 10px rgba(0, 0, 0, 0.02); 
        border: 4px solid transparent; 
        background-clip: padding-box; 
        position: relative; 
        overflow: hidden; 
      } 
       
      @keyframes gradientShift { 
        0%, 100% { background-position: 0% 50%; } 
        50% { background-position: 100% 50%; } 
      } 
       
      .main-header::before { 
        content: ''; 
        position: absolute; 
        top: -4px; 
        left: -4px; 
        right: -4px; 
        bottom: -4px; 
        background: linear-gradient(135deg,  
          #dc2626 0%,  
          #f97316 20%,  
          #fbbf24 40%,  
          #84cc16 60%,  
          #22c55e 80%,  
          #dc2626 100%); 
        background-size: 200% 200%; 
        animation: gradientShift 15s ease infinite; 
        border-radius: 28px; 
        z-index: -1; 
        opacity: 0.7; 
      } 
       
      .main-header::after { 
        content: '🍝'; 
        position: absolute; 
        font-size: 160px; 
        opacity: 0.05; 
        right: -35px; 
        top: 50%; 
        transform: translateY(-50%) rotate(-15deg); 
        animation: float 8s ease-in-out infinite; 
      } 
       
      @keyframes float { 
        0%, 100% { transform: translateY(-50%) rotate(-15deg); } 
        50% { transform: translateY(-55%) rotate(-12deg); } 
      } 
       
      .main-title { 
        font-size: 56px; 
        font-weight: 700; 
        background: linear-gradient(135deg, #dc2626 0%, #f97316 25%, #fbbf24 50%, #22c55e 75%, #dc2626 100%); 
        background-size: 200% auto; 
        -webkit-background-clip: text; 
        -webkit-text-fill-color: transparent; 
        background-clip: text; 
        margin: 0; 
        letter-spacing: -1px; 
        animation: shimmer 4s linear infinite; 
        position: relative; 
        z-index: 1; 
        text-align: center; 
      } 
       
      @keyframes shimmer { 
        to { background-position: 200% center; } 
      } 
       
      .main-subtitle { 
        font-size: 20px; 
        color: #78350f; 
        margin-top: 15px; 
        font-weight: 500; 
        opacity: 0.85; 
        position: relative; 
        z-index: 1; 
        text-align: center; 
      } 
       
      .stats-row { 
        display: grid; 
        grid-template-columns: repeat(auto-fit, minmax(220px, 1fr)); 
        gap: 20px; 
        margin-top: 30px; 
      } 
       
      .stats-card { 
        background: linear-gradient(135deg,  
          #fff1f2 0%,  
          #ffedd5 33%,  
          #fef3c7 66%,  
          #fff1f2 100%); 
        background-size: 200% 200%; 
        animation: gradientShift 10s ease infinite; 
        padding: 32px 28px; 
        border-radius: 22px; 
        text-align: center; 
        border: 4px solid rgba(251, 191, 36, 0.4); 
        box-shadow:  
          0 12px 30px rgba(249, 115, 22, 0.2), 
          inset 0 1px 0 rgba(255, 255, 255, 0.6); 
        position: relative; 
        overflow: hidden; 
        transition: all 0.4s cubic-bezier(0.4, 0, 0.2, 1); 
      } 
       
      .stats-card::before { 
        content: '✨'; 
        position: absolute; 
        font-size: 75px; 
        opacity: 0.08; 
        right: -18px; 
        top: -15px; 
        animation: sparkle 3s ease-in-out infinite; 
      } 
       
      @keyframes sparkle { 
        0%, 100% { opacity: 0.08; transform: scale(1) rotate(0deg); } 
        50% { opacity: 0.15; transform: scale(1.15) rotate(15deg); } 
      } 
       
      .stats-card:hover { 
        transform: translateY(-8px) scale(1.02); 
        box-shadow:  
          0 18px 45px rgba(249, 115, 22, 0.3), 
          inset 0 1px 0 rgba(255, 255, 255, 0.8); 
        border-color: #fbbf24; 
      } 
       
      .stats-number { 
        font-size: 62px; 
        font-weight: 700; 
        background: linear-gradient(135deg, #dc2626 0%, #f97316 50%, #fbbf24 100%); 
        -webkit-background-clip: text; 
        -webkit-text-fill-color: transparent; 
        background-clip: text; 
        line-height: 1; 
        margin-bottom: 10px; 
      } 
       
      .stats-label { 
        font-size: 16px; 
        color: #ea580c; 
        margin-top: 10px; 
        text-transform: uppercase; 
        letter-spacing: 2.5px; 
        font-weight: 700; 
      } 
       
      .action-buttons-row { 
        display: flex; 
        gap: 18px; 
        margin-top: 30px; 
        justify-content: center; 
        flex-wrap: wrap; 
      } 
       
      .btn-success, .btn-primary { 
        background: linear-gradient(135deg, #16a34a 0%, #22c55e 50%, #4ade80 100%); 
        background-size: 200% auto; 
        border: none; 
        padding: 18px 36px; 
        font-weight: 700; 
        font-size: 16px; 
        border-radius: 16px; 
        color: #ffffff; 
        box-shadow:  
          0 8px 24px rgba(34, 197, 94, 0.4), 
          inset 0 1px 0 rgba(255, 255, 255, 0.3); 
        transition: all 0.4s cubic-bezier(0.4, 0, 0.2, 1); 
        position: relative; 
        overflow: hidden; 
        text-transform: uppercase; 
        letter-spacing: 1px; 
      } 
       
      .btn-primary { 
        background: linear-gradient(135deg, #dc2626 0%, #f97316 50%, #fb923c 100%); 
        background-size: 200% auto; 
        box-shadow:  
          0 8px 24px rgba(220, 38, 38, 0.4), 
          inset 0 1px 0 rgba(255, 255, 255, 0.3); 
      } 
       
      .btn-secondary { 
        background: linear-gradient(135deg, #fef3c7 0%, #fde68a 50%, #fcd34d 100%); 
        background-size: 200% auto; 
        border: none; 
        padding: 18px 36px; 
        font-weight: 700; 
        font-size: 16px; 
        border-radius: 16px; 
        color: #78350f; 
        box-shadow:  
          0 8px 24px rgba(251, 191, 36, 0.4), 
          inset 0 1px 0 rgba(255, 255, 255, 0.3); 
        transition: all 0.4s cubic-bezier(0.4, 0, 0.2, 1); 
        position: relative; 
        overflow: hidden; 
        text-transform: uppercase; 
        letter-spacing: 1px; 
      } 
       
      .btn-danger { 
        background: linear-gradient(135deg, #dc2626 0%, #ef4444 50%, #f87171 100%); 
        background-size: 200% auto; 
        border: none; 
        padding: 18px 36px; 
        font-weight: 700; 
        font-size: 16px; 
        border-radius: 16px; 
        color: #ffffff; 
        box-shadow:  
          0 8px 24px rgba(220, 38, 38, 0.4), 
          inset 0 1px 0 rgba(255, 255, 255, 0.3); 
        transition: all 0.4s cubic-bezier(0.4, 0, 0.2, 1); 
        position: relative; 
        overflow: hidden; 
        text-transform: uppercase; 
        letter-spacing: 1px; 
      } 
       
      .btn-info { 
        background: linear-gradient(135deg, #0891b2 0%, #06b6d4 50%, #22d3ee 100%); 
        background-size: 200% auto; 
        border: none; 
        padding: 18px 36px; 
        font-weight: 700; 
        font-size: 16px; 
        border-radius: 16px; 
        color: #ffffff; 
        box-shadow:  
          0 8px 24px rgba(6, 182, 212, 0.4), 
          inset 0 1px 0 rgba(255, 255, 255, 0.3); 
        transition: all 0.4s cubic-bezier(0.4, 0, 0.2, 1); 
        position: relative; 
        overflow: hidden; 
        text-transform: uppercase; 
        letter-spacing: 1px; 
      } 
       
      .btn-success::before, .btn-primary::before, .btn-secondary::before, .btn-danger::before, .btn-info::before { 
        content: ''; 
        position: absolute; 
        top: 0; 
        left: -100%; 
        width: 100%; 
        height: 100%; 
        background: linear-gradient(90deg, transparent, rgba(255, 255, 255, 0.5), transparent); 
        transition: left 0.6s; 
      } 
       
      .btn-success:hover::before, .btn-primary:hover::before, .btn-secondary:hover::before, .btn-danger:hover::before, .btn-info:hover::before { 
        left: 100%; 
      } 
       
      .btn-success:hover { 
        animation: shimmer 1.5s linear infinite; 
        transform: translateY(-4px); 
        box-shadow:  
          0 14px 40px rgba(34, 197, 94, 0.5), 
          inset 0 1px 0 rgba(255, 255, 255, 0.5); 
      } 
       
      .btn-primary:hover { 
        animation: shimmer 1.5s linear infinite; 
        transform: translateY(-4px); 
        box-shadow:  
          0 14px 40px rgba(220, 38, 38, 0.5), 
          inset 0 1px 0 rgba(255, 255, 255, 0.5); 
      } 
       
      .btn-secondary:hover { 
        animation: shimmer 1.5s linear infinite; 
        transform: translateY(-4px); 
        box-shadow:  
          0 14px 40px rgba(251, 191, 36, 0.5), 
          inset 0 1px 0 rgba(255, 255, 255, 0.5); 
      } 
       
      .btn-danger:hover { 
        animation: shimmer 1.5s linear infinite; 
        transform: translateY(-4px); 
        box-shadow:  
          0 14px 40px rgba(220, 38, 38, 0.5), 
          inset 0 1px 0 rgba(255, 255, 255, 0.5); 
      } 
       
      .btn-info:hover { 
        animation: shimmer 1.5s linear infinite; 
        transform: translateY(-4px); 
        box-shadow:  
          0 14px 40px rgba(6, 182, 212, 0.5), 
          inset 0 1px 0 rgba(255, 255, 255, 0.5); 
      } 
       
      .dashboard-content { 
        background: linear-gradient(135deg,  
          #fef2f2 0%,  
          #fff7ed 20%,  
          #fefce8 40%,  
          #f7fee7 60%,  
          #ecfdf5 80%,  
          #fef2f2 100%); 
        background-size: 200% 200%; 
        animation: gradientShift 15s ease infinite; 
        padding: 40px; 
        border-radius: 28px; 
        margin-top: 35px; 
        box-shadow:  
          0 25px 70px rgba(0, 0, 0, 0.12), 
          0 8px 20px rgba(0, 0, 0, 0.06), 
          inset 0 2px 0 rgba(255, 255, 255, 0.8); 
        border: 4px solid rgba(249, 115, 22, 0.2); 
        position: relative; 
        overflow: hidden; 
      } 
       
      .dashboard-content::before { 
        content: '🍅'; 
        position: absolute; 
        font-size: 150px; 
        opacity: 0.03; 
        left: -35px; 
        bottom: -35px; 
        transform: rotate(-20deg); 
        animation: float 10s ease-in-out infinite reverse; 
      } 
       
      .dashboard-content::after { 
        content: '🥕'; 
        position: absolute; 
        font-size: 100px; 
        opacity: 0.03; 
        right: -25px; 
        top: -25px; 
        transform: rotate(30deg); 
        animation: float 12s ease-in-out infinite; 
      } 
       
      .meal-plan-section { 
        background: linear-gradient(135deg,  
          #fff5f5 0%,  
          #fffbeb 20%,  
          #fef3c7 40%,  
          #ecfccb 60%,  
          #d1fae5 80%,  
          #fff5f5 100%); 
        background-size: 200% 200%; 
        animation: gradientShift 15s ease infinite; 
        padding: 50px 45px; 
        border-radius: 28px; 
        border: 4px solid transparent; 
        background-clip: padding-box; 
        box-shadow:  
          0 25px 70px rgba(0, 0, 0, 0.12), 
          0 8px 20px rgba(0, 0, 0, 0.06), 
          inset 0 2px 0 rgba(255, 255, 255, 0.8), 
          inset 0 -2px 10px rgba(0, 0, 0, 0.02); 
        position: relative; 
        overflow: hidden; 
      } 
       
      .meal-plan-section::before { 
        content: ''; 
        position: absolute; 
        top: -4px; 
        left: -4px; 
        right: -4px; 
        bottom: -4px; 
        background: linear-gradient(135deg,  
          #dc2626 0%,  
          #f97316 20%,  
          #fbbf24 40%,  
          #84cc16 60%,  
          #22c55e 80%,  
          #dc2626 100%); 
        background-size: 200% 200%; 
        animation: gradientShift 15s ease infinite; 
        border-radius: 28px; 
        z-index: -1; 
        opacity: 0.7; 
      } 
       
      .meal-plan-section::after { 
        content: '🥗'; 
        position: absolute; 
        font-size: 160px; 
        opacity: 0.05; 
        right: -35px; 
        top: 50%; 
        transform: translateY(-50%) rotate(20deg); 
        animation: float 9s ease-in-out infinite; 
      } 
       
      .meal-plan-header { 
        font-size: 56px; 
        font-weight: 700; 
        background: linear-gradient(135deg, #dc2626 0%, #f97316 25%, #fbbf24 50%, #22c55e 75%, #dc2626 100%); 
        background-size: 200% auto; 
        -webkit-background-clip: text; 
        -webkit-text-fill-color: transparent; 
        background-clip: text; 
        animation: shimmer 4s linear infinite; 
        margin-bottom: 15px; 
        position: relative; 
        z-index: 1; 
        text-align: center; 
        letter-spacing: -1px; 
      } 
       
      .meal-plan-description { 
        font-size: 20px; 
        color: #78350f; 
        margin-top: 15px; 
        margin-bottom: 30px; 
        font-weight: 500; 
        opacity: 0.85; 
        position: relative; 
        z-index: 1; 
        text-align: center; 
      } 
       
      .time-input-group { 
        display: grid; 
        grid-template-columns: repeat(auto-fit, minmax(180px, 1fr)); 
        gap: 12px; 
        margin-bottom: 15px; 
        margin-top: 20px; 
        align-items: start; 
      } 
      
      .filter-input-group { 
        display: grid; 
        grid-template-columns: repeat(auto-fit, minmax(160px, 1fr)); 
        gap: 12px; 
        margin-bottom: 20px;
        margin-left: 0; /* remove negative margin */
        align-items: start; 
      } 
     
    .time-input-group .form-group, 
    .filter-input-group .form-group { 
      display: flex; 
      flex-direction: column; 
      height: 100%; 
    } 
     
    .time-input-group .form-group label, 
    .filter-input-group .form-group label { 
      min-height: 30px; 
      display: flex; 
      align-items: center; 
      font-size: 13px; 
    } 
     
    .time-input-group .form-control, 
    .time-input-group .selectize-input, 
    .filter-input-group .form-control, 
    .filter-input-group .selectize-input { 
      padding: 10px 12px !important; 
      font-size: 14px !important; 
    } 
     
    .time-input-group .btn { 
      margin-top: auto; 
      padding: 12px 24px !important; 
      font-size: 14px !important; 
    } 
       
      @media (max-width: 768px) { 
        .time-input-group { 
          grid-template-columns: 1fr; 
          align-items: stretch; 
        } 
      } 
       
      .form-group label { 
        color: #78350f; 
        font-weight: 700; 
        font-size: 15px; 
        letter-spacing: 0.5px; 
        margin-bottom: 10px; 
        display: block; 
      } 
       
      .form-control, .selectize-input { 
        background: linear-gradient(135deg, #ffffff 0%, #fffef9 100%) !important; 
        border: 3px solid #fed7aa !important; 
        color: #1c1917 !important; 
        border-radius: 14px; 
        padding: 14px 18px; 
        transition: all 0.3s cubic-bezier(0.4, 0, 0.2, 1); 
        font-size: 16px; 
        font-weight: 600; 
      } 
       
      .form-control:focus, .selectize-input.focus { 
        border-color: #f97316 !important; 
        box-shadow:  
          0 0 0 5px rgba(249, 115, 22, 0.15), 
          0 6px 16px rgba(249, 115, 22, 0.2) !important; 
        background: #ffffff !important; 
        transform: translateY(-2px); 
      } 
       
      .combination-card { 
        background: linear-gradient(135deg,  
          #fff7ed 0%,  
          #ffedd5 50%,  
          #fed7aa 100%); 
        background-size: 200% 200%; 
        animation: gradientShift 10s ease infinite; 
        padding: 28px; 
        border-radius: 20px; 
        margin-bottom: 22px; 
        border: 4px solid #fbbf24; 
        box-shadow:  
          0 10px 30px rgba(249, 115, 22, 0.18), 
          inset 0 1px 0 rgba(255, 255, 255, 0.5); 
        transition: all 0.4s cubic-bezier(0.4, 0, 0.2, 1); 
        position: relative; 
        overflow: hidden; 
      } 
       
      .combination-card::before { 
        content: '🌿'; 
        position: absolute; 
        font-size: 90px; 
        opacity: 0.06; 
        right: 18px; 
        top: 50%; 
        transform: translateY(-50%) rotate(25deg); 
      } 
       
      .combination-card:hover { 
        transform: translateY(-6px) scale(1.01); 
        box-shadow:  
          0 18px 45px rgba(249, 115, 22, 0.28), 
          inset 0 1px 0 rgba(255, 255, 255, 0.7); 
        border-color: #f97316; 
      } 
       
      .combination-header { 
        font-size: 24px; 
        font-weight: 700; 
        color: #c2410c; 
        margin-bottom: 22px; 
        display: flex; 
        align-items: center; 
        justify-content: space-between; 
        position: relative; 
        z-index: 1; 
      } 
       
      .recipe-chip { 
        display: inline-flex; 
        align-items: center; 
        gap: 16px; 
        background: linear-gradient(135deg, #ffffff 0%, #fffef9 100%); 
        padding: 16px 22px; 
        border-radius: 18px; 
        margin: 10px; 
        color: #1c1917; 
        font-size: 16px; 
        font-weight: 600; 
        cursor: pointer; 
        transition: all 0.3s cubic-bezier(0.4, 0, 0.2, 1); 
        border: 3px solid #fde68a; 
        box-shadow: 0 6px 16px rgba(251, 191, 36, 0.2); 
        position: relative; 
        overflow: hidden; 
      } 
       
      .recipe-chip::before { 
        content: ''; 
        position: absolute; 
        top: 0; 
        left: -100%; 
        width: 100%; 
        height: 100%; 
        background: linear-gradient(90deg, transparent, rgba(249, 115, 22, 0.2), transparent); 
        transition: left 0.6s; 
      } 
       
      .recipe-chip:hover::before { 
        left: 100%; 
      } 
       
      .recipe-chip:hover { 
        background: linear-gradient(135deg, #fef3c7 0%, #fde68a 100%); 
        border-color: #f97316; 
        transform: translateY(-5px) scale(1.04); 
        box-shadow: 0 16px 35px rgba(249, 115, 22, 0.3); 
      } 
       
      .recipe-chip-image { 
        width: 60px; 
        height: 60px; 
        border-radius: 14px; 
        object-fit: cover; 
        border: 3px solid #fde68a; 
        box-shadow: 0 4px 12px rgba(0, 0, 0, 0.15); 
      } 
       
      .recipe-chip-placeholder { 
        width: 60px; 
        height: 60px; 
        background: linear-gradient(135deg, #fef3c7 0%, #fde68a 100%); 
        border-radius: 14px; 
        display: flex; 
        align-items: center; 
        justify-content: center; 
        font-size: 30px; 
        border: 3px solid #fbbf24; 
      } 
       
      .recipe-chip .category-badge { 
        background: linear-gradient(135deg, #dc2626 0%, #f97316 100%); 
        color: #ffffff; 
        padding: 6px 16px; 
        border-radius: 12px; 
        font-size: 13px; 
        margin-left: 12px; 
        font-weight: 700; 
        box-shadow: 0 3px 10px rgba(220, 38, 38, 0.4); 
        text-transform: uppercase; 
        letter-spacing: 0.5px; 
      } 
       
      .time-badge { 
        background: linear-gradient(135deg, #16a34a 0%, #22c55e 100%); 
        color: white; 
        padding: 12px 24px; 
        border-radius: 14px; 
        font-size: 17px; 
        font-weight: 700; 
        display: inline-block; 
        box-shadow: 0 6px 16px rgba(34, 197, 94, 0.4); 
      } 
       
      .no-results { 
        color: #78350f; 
        text-align: center; 
        padding: 60px; 
        font-style: italic; 
        font-size: 18px; 
        font-weight: 500; 
        opacity: 0.7; 
      } 
       
      /* Decorative stickers */ 
      .sticker { 
        position: absolute; 
        font-size: 60px; 
        opacity: 0.25; 
        transform: rotate(var(--rotation)); 
        animation: floatSticker 6s ease-in-out infinite; 
        z-index: 2; 
        filter: drop-shadow(0 4px 8px rgba(0, 0, 0, 0.1)); 
      } 
       
      @keyframes floatSticker { 
        0%, 100% { transform: translateY(0) rotate(var(--rotation)); } 
        50% { transform: translateY(-15px) rotate(calc(var(--rotation) + 5deg)); } 
      } 
       
      .sticker-1 { 
        --rotation: -12deg; 
        top: 15%; 
        left: 5%; 
      } 
       
      .sticker-2 { 
        --rotation: 15deg; 
        top: 25%; 
        right: 8%; 
      } 
       
      .sticker-3 { 
        --rotation: -8deg; 
        bottom: 20%; 
        left: 8%; 
      } 
       
      .sticker-4 { 
        --rotation: 18deg; 
        bottom: 15%; 
        right: 6%; 
      } 
       
      .sticker-5 { 
        --rotation: -15deg; 
        top: 50%; 
        left: 3%; 
      } 
       
      .sticker-6 { 
        --rotation: 12deg; 
        top: 60%; 
        right: 4%; 
      } 
 
      /* Recipe cards get fun stickers */ 
      .combination-card .mini-sticker { 
        position: absolute; 
        font-size: 35px; 
        opacity: 0.15; 
        animation: rotate 4s linear infinite; 
      } 
       
      @keyframes rotate { 
        from { transform: rotate(0deg); } 
        to { transform: rotate(360deg); } 
      } 
       
      .combination-card .mini-sticker-1 { 
        top: 10px; 
        right: 80px; 
      } 
       
      .combination-card .mini-sticker-2 { 
        bottom: 10px; 
        left: 80px; 
      } 
       
      /* Stats card decorative elements */ 
      .stats-card .corner-decoration { 
        position: absolute; 
        width: 30px; 
        height: 30px; 
        border: 3px solid #f97316; 
        opacity: 0.3; 
      } 
       
      .stats-card .corner-decoration.top-left { 
        top: 10px; 
        left: 10px; 
        border-right: none; 
        border-bottom: none; 
        border-radius: 8px 0 0 0; 
      } 
       
      .stats-card .corner-decoration.bottom-right { 
        bottom: 10px; 
        right: 10px; 
        border-left: none; 
        border-top: none; 
        border-radius: 0 0 8px 0; 
      } 
       
      /* Confetti-like dots */ 
      .confetti-dot { 
        position: absolute; 
        width: 8px; 
        height: 8px; 
        border-radius: 50%; 
        opacity: 0.3; 
        animation: confettiFall 8s ease-in-out infinite; 
      } 
       
      @keyframes confettiFall { 
        0%, 100% { transform: translateY(0) rotate(0deg); } 
        50% { transform: translateY(-20px) rotate(180deg); } 
      } 
       
      .dot-1 { background: #dc2626; top: 20%; left: 15%; animation-delay: 0s; } 
      .dot-2 { background: #f97316; top: 35%; right: 20%; animation-delay: 1s; } 
      .dot-3 { background: #fbbf24; bottom: 30%; left: 25%; animation-delay: 2s; } 
      .dot-4 { background: #22c55e; bottom: 45%; right: 15%; animation-delay: 3s; } 
      .dot-5 { background: #dc2626; top: 60%; left: 10%; animation-delay: 4s; } 
      .dot-6 { background: #f97316; top: 75%; right: 18%; animation-delay: 5s; } 
       
      /* Hand-drawn style arrows */ 
      .doodle-arrow { 
        position: absolute; 
        width: 80px; 
        height: 60px; 
        opacity: 0.15; 
        z-index: 2; 
      } 
       
      .arrow-1 { 
        top: 18%; 
        right: 25%; 
        transform: rotate(-45deg); 
      } 
       
      .arrow-2 { 
        bottom: 25%; 
        left: 22%; 
        transform: rotate(135deg); 
      } 
       
      /* Modal styling */ 
      .modal-content { 
        background: linear-gradient(135deg,  
          #fff5f5 0%,  
          #fffbeb 20%,  
          #fef3c7 40%,  
          #ecfccb 60%,  
          #d1fae5 80%,  
          #fff5f5 100%) !important; 
        background-size: 200% 200%; 
        animation: gradientShift 15s ease infinite; 
        border-radius: 28px !important; 
        border: 4px solid transparent !important; 
        box-shadow:  
          0 25px 70px rgba(0, 0, 0, 0.15), 
          0 8px 20px rgba(0, 0, 0, 0.08) !important; 
        position: relative; 
        overflow: hidden; 
      } 
       
      .modal-content::before { 
        content: ''; 
        position: absolute; 
        top: -4px; 
        left: -4px; 
        right: -4px; 
        bottom: -4px; 
        background: linear-gradient(135deg,  
          #dc2626 0%,  
          #f97316 20%,  
          #fbbf24 40%,  
          #84cc16 60%,  
          #22c55e 80%,  
          #dc2626 100%); 
        background-size: 200% 200%; 
        animation: gradientShift 15s ease infinite; 
        border-radius: 28px; 
        z-index: -1; 
        opacity: 0.7; 
      } 
       
      .modal-header { 
        background: transparent !important; 
        border-bottom: 3px solid rgba(249, 115, 22, 0.3) !important; 
        padding: 30px 35px !important; 
        position: relative; 
      } 
       
      .modal-header::after { 
        content: '🍽️'; 
        position: absolute; 
        font-size: 70px; 
        opacity: 0.08; 
        right: 20px; 
        top: 50%; 
        transform: translateY(-50%) rotate(-15deg); 
      } 
       
      .modal-title { 
        font-size: 38px; 
        font-weight: 700; 
        background: linear-gradient(135deg, #dc2626 0%, #f97316 50%, #fbbf24 100%); 
        -webkit-background-clip: text; 
        -webkit-text-fill-color: transparent; 
        background-clip: text; 
        position: relative; 
        z-index: 1; 
      } 
       
      .modal-body { 
        background: transparent !important; 
        padding: 35px !important; 
        color: #1c1917 !important; 
        font-size: 16px; 
        font-weight: 600; 
        position: relative; 
        z-index: 1; 
      } 
       
      .modal-footer { 
        background: transparent !important; 
        border-top: 3px solid rgba(249, 115, 22, 0.3) !important; 
        padding: 25px 35px !important; 
      } 
       
      .modal-footer .btn, .modal-header .close { 
        background: linear-gradient(135deg, #fef3c7 0%, #fde68a 50%, #fcd34d 100%); 
        background-size: 200% auto; 
        border: none; 
        padding: 18px 36px; 
        font-weight: 700; 
        font-size: 16px; 
        border-radius: 16px; 
        color: #78350f; 
        box-shadow:  
          0 8px 24px rgba(251, 191, 36, 0.4), 
          inset 0 1px 0 rgba(255, 255, 255, 0.3); 
        transition: all 0.4s cubic-bezier(0.4, 0, 0.2, 1); 
        position: relative; 
        overflow: hidden; 
        text-transform: uppercase; 
        letter-spacing: 1px; 
        opacity: 1; 
      } 
       
      .modal-footer .btn-primary { 
        background: linear-gradient(135deg, #16a34a, #22c55e); 
        color: #ffffff; 
      } 
 
     .modal-footer .btn-secondary { 
       background: linear-gradient(135deg, #dc2626, #f97316); 
       color: #ffffff; 
      } 
       
      .modal-footer .btn::before, .modal-header .close::before { 
        content: ''; 
        position: absolute; 
        top: 0; 
        left: -100%; 
        width: 100%; 
        height: 100%; 
        background: linear-gradient(90deg, transparent, rgba(255, 255, 255, 0.5), transparent); 
        transition: left 0.6s; 
      } 
       
      .modal-footer .btn:hover::before, .modal-header .close:hover::before { 
        left: 100%; 
      } 
       
      .modal-footer .btn:hover, .modal-header .close:hover { 
        animation: shimmer 1.5s linear infinite; 
        transform: translateY(-4px); 
        box-shadow:  
          0 14px 40px rgba(251, 191, 36, 0.5), 
          inset 0 1px 0 rgba(255, 255, 255, 0.5); 
      } 
       
      /* Recipe detail styling to match form fonts */ 
      .recipe-detail-section { 
        margin-bottom: 28px; 
      } 
       
      .recipe-detail-label { 
        color: #c2410c; 
        font-weight: 700; 
        font-size: 15px; 
        text-transform: uppercase; 
        letter-spacing: 0.5px; 
        margin-bottom: 10px; 
        display: block; 
      } 
       
      .recipe-detail-content { 
        color: #1c1917; 
        background: linear-gradient(135deg, #ffffff 0%, #fffef9 100%); 
        padding: 14px 18px; 
        border-radius: 14px; 
        white-space: pre-wrap; 
        border: 3px solid #fed7aa; 
        line-height: 1.8; 
        font-size: 16px; 
        font-weight: 600; 
        box-shadow: inset 0 2px 4px rgba(0, 0, 0, 0.02); 
      } 
       
      .recipe-detail-image { 
        width: 100%; 
        max-width: 400px; 
        border-radius: 16px; 
        margin-bottom: 28px; 
        border: 4px solid #f97316; 
        box-shadow: 0 8px 24px rgba(249, 115, 22, 0.2); 
      } 
       
      .form-panel, .list-panel { 
        background: linear-gradient(135deg,  
          #fff5f5 0%,  
          #fffbeb 20%,  
          #fef3c7 40%,  
          #ecfccb 60%,  
          #d1fae5 80%,  
          #fff5f5 100%); 
        background-size: 200% 200%; 
        animation: gradientShift 15s ease infinite; 
        padding: 0; 
        border-radius: 28px; 
        margin-bottom: 35px; 
        box-shadow:  
          0 25px 70px rgba(0, 0, 0, 0.12), 
          0 8px 20px rgba(0, 0, 0, 0.06), 
          inset 0 2px 0 rgba(255, 255, 255, 0.8); 
        border: 4px solid transparent; 
        background-clip: padding-box; 
        position: relative; 
        overflow: hidden; 
      } 
       
      .form-panel::before, .list-panel::before { 
        content: ''; 
        position: absolute; 
        top: -4px; 
        left: -4px; 
        right: -4px; 
        bottom: -4px; 
        background: linear-gradient(135deg,  
          #dc2626 0%,  
          #f97316 20%,  
          #fbbf24 40%,  
          #84cc16 60%,  
          #22c55e 80%,  
          #dc2626 100%); 
        background-size: 200% 200%; 
        animation: gradientShift 15s ease infinite; 
        border-radius: 28px; 
        z-index: -1; 
        opacity: 0.7; 
      } 
       
      .form-panel::after { 
        content: '🍳'; 
        position: absolute; 
        font-size: 160px; 
        opacity: 0.05; 
        right: -35px; 
        top: 50%; 
        transform: translateY(-50%) rotate(-15deg); 
        animation: float 8s ease-in-out infinite; 
      } 
       
      .list-panel::after { 
        content: '📖'; 
        position: absolute; 
        font-size: 160px; 
        opacity: 0.05; 
        right: -35px; 
        top: 50%; 
        transform: translateY(-50%) rotate(15deg); 
        animation: float 10s ease-in-out infinite; 
      } 
       
      .panel-header { 
        background: transparent !important; 
        padding: 30px 35px; 
        font-size: 56px; 
        font-weight: 700; 
        background: linear-gradient(135deg, #dc2626 0%, #f97316 25%, #fbbf24 50%, #22c55e 75%, #dc2626 100%); 
        background-size: 200% auto; 
        -webkit-background-clip: text; 
        -webkit-text-fill-color: transparent; 
        background-clip: text; 
        animation: shimmer 4s linear infinite; 
        border-bottom: 3px solid rgba(249, 115, 22, 0.3); 
        position: relative; 
        z-index: 1; 
        letter-spacing: -1px; 
      } 
       
      .panel-content { 
        padding: 35px 40px; 
        background: transparent; 
        position: relative; 
        z-index: 1; 
      } 
       
      /* Selectize dropdown */ 
      .selectize-dropdown { 
        background: linear-gradient(135deg, #ffffff 0%, #fffef9 100%); 
        border: 3px solid #fed7aa; 
        border-radius: 14px; 
        box-shadow: 0 16px 40px rgba(249, 115, 22, 0.25); 
      } 
       
      .selectize-dropdown .option { 
        padding: 14px 18px; 
        color: #1c1917; 
        font-size: 16px; 
        font-weight: 600; 
        transition: all 0.2s; 
      } 
       
      .selectize-dropdown .option:hover { 
        background: linear-gradient(135deg, #fff7ed 0%, #ffedd5 100%); 
        color: #c2410c; 
      } 
       
      .selectize-dropdown .active { 
        background: linear-gradient(135deg, #fed7aa 0%, #fdba74 100%); 
        color: #c2410c; 
        font-weight: 700; 
      } 
    ")) 
  ), 
  
  uiOutput("mainUI") 
) 

# ------------------------- 
# Server 
# ------------------------- 
server <- function(input, output, session) { 
  
  rv <- reactiveValues( 
    user = NULL, 
    recipes = data.frame(), 
    currentEditID = NULL, 
    imagePath = "", 
    refresh = 0, 
    mealCombinations = list() 
  ) 
  
  observe({ 
    init_db() 
  }) 
  
  output$mainUI <- renderUI({ 
    if (is.null(rv$user)) { 
      # Login UI 
      div(class = "login-container", 
          # Decorative stickers for login 
          tags$div(class = "sticker sticker-1", "🍅"), 
          tags$div(class = "sticker sticker-2", "🥕"), 
          tags$div(class = "sticker sticker-4", "🌿"), 
          
          # Confetti dots 
          tags$div(class = "confetti-dot dot-1"), 
          tags$div(class = "confetti-dot dot-2"), 
          tags$div(class = "confetti-dot dot-4"), 
          
          h1(class = "login-title", "🍳 Recipe Manager"), 
          p(class = "login-subtitle", "Please login or register to continue"), 
          
          tabsetPanel( 
            id = "authTabs", 
            type = "tabs", 
            tabPanel("Login", 
                     div(style = "position: relative; z-index: 1; padding-top: 20px;", 
                         textInput("loginUsername", "Username", placeholder = "Enter username", width = "100%"), 
                         passwordInput("loginPassword", "Password", placeholder = "Enter password", width = "100%"), 
                         br(), 
                         actionButton("loginBtn", "🔐 Login", class = "btn-success", style = "width: 100%;") 
                     ) 
            ), 
            tabPanel("Register", 
                     div(style = "position: relative; z-index: 1; padding-top: 20px;", 
                         textInput("regFullName", "Full Name", placeholder = "Enter full name", width = "100%"), 
                         textInput("regUsername", "Username", placeholder = "Choose username", width = "100%"), 
                         textInput("regEmail", "Email", placeholder = "Enter email", width = "100%"), 
                         passwordInput("regPassword", "Password", placeholder = "Choose password", width = "100%"), 
                         passwordInput("regConfirmPassword", "Confirm Password", placeholder = "Confirm password", width = "100%"), 
                         br(), 
                         actionButton("registerBtn", "📝 Register", class = "btn-primary", style = "width: 100%;") 
                     ) 
            ) 
          ) 
      ) 
    } else { 
      tagList( 
        div(class = "main-header", 
            # Decorative stickers 
            tags$div(class = "sticker sticker-1", "🍅"), 
            tags$div(class = "sticker sticker-2", "🥕"), 
            tags$div(class = "sticker sticker-3", "🧄"), 
            tags$div(class = "sticker sticker-4", "🌿"), 
            tags$div(class = "sticker sticker-5", "🧅"), 
            tags$div(class = "sticker sticker-6", "🫑"), 
            
            # Confetti dots 
            tags$div(class = "confetti-dot dot-1"), 
            tags$div(class = "confetti-dot dot-2"), 
            tags$div(class = "confetti-dot dot-3"), 
            tags$div(class = "confetti-dot dot-4"), 
            tags$div(class = "confetti-dot dot-5"), 
            tags$div(class = "confetti-dot dot-6"), 
            
            # User info - positioned to the left 
            div(style = "position: absolute; top: 20px; right: 20px; z-index: 10; display: flex; align-items: center; gap: 10px;", 
                div(style = "color: #78350f; font-weight: 600; font-size: 16px;", paste("👤", rv$user$username)), 
                actionButton("logoutBtn", "Logout", class = "btn-secondary", style = "padding: 8px 16px; font-size: 14px;") 
            ), 
            
            
            h1(class = "main-title", "🍳 Recipe Manager"), 
            p(class = "main-subtitle", "Organize your culinary creations with style!"), 
            
            div(class = "stats-row", 
                div(class = "stats-card", 
                    tags$div(class = "corner-decoration top-left"), 
                    tags$div(class = "corner-decoration bottom-right"), 
                    div(class = "stats-number", textOutput("totalRecipes", inline = TRUE)), 
                    div(class = "stats-label", "Total Recipes") 
                ) 
            ), 
            div(class = "action-buttons-row", 
                actionButton("openFormBtn", "➕ Add Recipe", class = "btn-success"), 
                actionButton("openCollectionBtn", "📚 My Recipes", class = "btn-primary") 
            ) 
        ), 
        
        div(class = "dashboard-content", 
            div(class = "meal-plan-section", 
                # Decorative stickers for meal planner 
                tags$div(class = "sticker sticker-2", "🥕"), 
                tags$div(class = "sticker sticker-4", "🌿"), 
                tags$div(class = "sticker sticker-5", "🧅"), 
                
                # Confetti dots 
                tags$div(class = "confetti-dot dot-2"), 
                tags$div(class = "confetti-dot dot-4"), 
                tags$div(class = "confetti-dot dot-5"), 
                
                div(class = "meal-plan-header", "⏰ Smart Meal Planner"), 
                p(class = "meal-plan-description",  
                  "Enter your available time and filter by combination type to find perfect meal matches! Click on any dish to view its recipe."), 
                
                div(class = "time-input-group", 
                    # First row - Time inputs and Find button 
                    div(style = "display: flex; flex-direction: column;", 
                        numericInput("targetTime", "Available Time (minutes)", value = 60, min = 1, max = 480, step = 5) 
                    ), 
                    div(style = "display: flex; flex-direction: column;", 
                        numericInput("timeTolerance", "Tolerance (± minutes)", value = 10, min = 0, max = 30, step = 5) 
                    ), 
                    div(style = "display: flex; flex-direction: column; justify-content: flex-end;", 
                        tags$label(style = "min-height: 30px; visibility: hidden;", "Placeholder"), 
                        actionButton("findMealsBtn", "🔍 Find", class = "btn-primary") 
                    ) 
                ), 
                
                # Second row - Filter dropdowns 
                div(class = "filter-input-group", 
                    div(style = "display: flex; flex-direction: column;", 
                        selectInput("difficultyFilter", "Difficulty", 
                                    choices = c( 
                                      "Any" = "any", 
                                      "Easy" = "Easy", 
                                      "Medium" = "Medium", 
                                      "Hard" = "Hard" 
                                    ), 
                                    selected = "any") 
                    ), 
                    div(style = "display: flex; flex-direction: column;", 
                        selectInput("singleFilter", "Single Dish", 
                                    choices = c( 
                                      "None" = "", 
                                      "Main" = "single_Main", 
                                      "Side" = "single_Side", 
                                      "Appetizer" = "single_Appetizer", 
                                      "Snack" = "single_Snack", 
                                      "Dessert" = "single_Dessert", 
                                      "Drink" = "single_Drink" 
                                    ), 
                                    selected = "") 
                    ), 
                    div(style = "display: flex; flex-direction: column;", 
                        selectInput("pairingFilter", "Pairing (2-Dish)", 
                                    choices = c( 
                                      "None" = "", 
                                      "Main + Side" = "Main+Side", 
                                      "Main + Appetizer" = "Main+Appetizer", 
                                      "Main + Drink" = "Main+Drink", 
                                      "Main + Dessert" = "Main+Dessert", 
                                      "Snack + Drink" = "Snack+Drink", 
                                      "Dessert + Drink" = "Dessert+Drink" 
                                    ), 
                                    selected = "") 
                    ), 
                    div(style = "display: flex; flex-direction: column;", 
                        selectInput("setFilter", "Set (3-Dish)", 
                                    choices = c( 
                                      "None" = "", 
                                      "Appetizer + Main + Side" = "Appetizer+Main+Side", 
                                      "Main + Side + Drink" = "Main+Side+Drink", 
                                      "Main + Side + Dessert" = "Main+Side+Dessert", 
                                      "Appetizer + Main + Dessert" = "Appetizer+Main+Dessert", 
                                      "Main + Drink + Dessert" = "Main+Drink+Dessert", 
                                      "Snack + Drink + Dessert" = "Snack+Drink+Dessert" 
                                    ), 
                                    selected = "") 
                    ) 
                ), 
                
                uiOutput("mealCombinations") 
            ) 
        ) 
      ) 
    } 
  }) 
  
  # Login handler 
  observeEvent(input$loginBtn, { 
    username <- trimws(input$loginUsername) 
    password <- input$loginPassword 
    
    if (username == "" || password == "") { 
      showNotification("Please enter username and password", type = "warning") 
      return() 
    } 
    
    user <- authenticate_user(username, password) 
    
    if (!is.null(user)) { 
      rv$user <- user 
      rv$recipes <- load_recipes(user$id) 
      showNotification(paste("Welcome,", user$username, "!"), type = "message") 
    } else { 
      showNotification("Invalid username or password", type = "error") 
    } 
  }) 
  
  # Register handler 
  observeEvent(input$registerBtn, { 
    fullName <- trimws(input$regFullName) 
    username <- trimws(input$regUsername) 
    email <- trimws(input$regEmail) 
    password <- input$regPassword 
    confirmPassword <- input$regConfirmPassword 
    
    if (username == "" || email == "" || password == "") { 
      showNotification("Please fill in all required fields", type = "warning") 
      return() 
    } 
    
    if (password != confirmPassword) { 
      showNotification("Passwords do not match", type = "warning") 
      return() 
    } 
    
    if (nchar(password) < 6) { 
      showNotification("Password must be at least 6 characters", type = "warning") 
      return() 
    } 
    
    result <- create_user(username, email, password, fullName) 
    
    if (result$success) { 
      showNotification("Registration successful! Please login.", type = "message") 
      updateTabsetPanel(session, "authTabs", selected = "Login") 
    } else { 
      showNotification(result$message, type = "error") 
    } 
  }) 
  
  # Logout handler 
  observeEvent(input$logoutBtn, { 
    rv$user <- NULL 
    rv$recipes <- data.frame() 
    rv$currentEditID <- NULL 
    rv$imagePath <- "" 
    rv$mealCombinations <- list() 
    showNotification("Logged out successfully", type = "message") 
  }) 
  
  # Load recipes when user changes 
  observe({ 
    if (!is.null(rv$user)) { 
      rv$refresh 
      rv$recipes <- load_recipes(rv$user$id) 
    } 
  }) 
  
  # Total recipes count 
  output$totalRecipes <- renderText({ 
    if (is.null(rv$user)) return("0") 
    as.character(nrow(rv$recipes)) 
  }) 
  
  # Image preview 
  output$imagePreview <- renderText({ 
    if (is.null(input$imageFile)) { 
      if (rv$imagePath != "") { 
        return(paste("Current:", rv$imagePath)) 
      } 
      return("No image selected") 
    } 
    paste("Selected:", input$imageFile$name) 
  }) 
  
  # Status message 
  output$statusMessage <- renderText({ 
    if (!is.null(rv$currentEditID)) { 
      paste("Editing Recipe ID:", rv$currentEditID) 
    } else { 
      "" 
    } 
  }) 
  
  # Open form modal 
  observeEvent(input$openFormBtn, { 
    if (is.null(rv$user)) return() 
    
    rv$currentEditID <- NULL 
    rv$imagePath <- "" 
    
    showModal(modalDialog( 
      title = "✨ Add New Recipe", 
      size = "l", 
      
      textInput("recipeName", "Recipe Name *", placeholder = "Enter recipe name", value = ""), 
      selectInput("category", "Category *",  
                  choices = c("Main","Side","Appetizer","Snack","Dessert","Drink"), 
                  selected = "Main"), 
      selectInput("difficulty", "Difficulty Level *", 
                  choices = c("Easy", "Medium", "Hard"), 
                  selected = "Easy"), 
      
      fluidRow( 
        column(6, numericInput("prepTime", "Prep (min)", value = 0, min = 0)),
        column(6, numericInput("cookTime", "Cook (min)", value = 0, min = 0))
      ), 
      
      textAreaInput("ingredients", "Ingredients *", height = "150px", 
                    placeholder = "List ingredients, one per line", value = ""), 
      textAreaInput("instructions", "Instructions *", height = "150px", 
                    placeholder = "Step-by-step instructions", value = ""), 
      textInput("tags", "Tags", placeholder = "e.g., healthy, quick", value = ""), 
      
      fileInput("imageFile", "Recipe Image", accept = c('image/png', 'image/jpeg', 'image/jpg')), 
      textOutput("imagePreview"), 
      br(), 
      textOutput("statusMessage"), 
      
      footer = tagList( 
        modalButton("Cancel"), 
        actionButton("saveBtn", "💾 Save Recipe", class = "btn-primary"), 
        actionButton("clearBtn", "🗑 Clear Form", class = "btn-secondary") 
      ), 
      easyClose = FALSE 
    )) 
  }) 
  
  # Open collection modal 
  observeEvent(input$openCollectionBtn, { 
    if (is.null(rv$user)) return() 
    
    showModal(modalDialog( 
      title = "📚 Recipe Collection", 
      size = "l", 
      
      fluidRow( 
        column(12, 
               textInput("searchBox", NULL, placeholder = "🔍 Search recipes...", width = "100%"), 
               br(), 
               DTOutput("recipeTable"), 
               br(), 
               actionButton("viewBtn", "👁 View Details", class = "btn-secondary"), 
               actionButton("editBtn", "✏ Edit Recipe", class = "btn-info"), 
               actionButton("deleteBtn", "🗑 Delete Recipe", class = "btn-danger") 
        ) 
      ), 
      
      easyClose = TRUE, 
      footer = modalButton("Close") 
    )) 
  }) 
  
  # Display recipe table 
  output$recipeTable <- renderDT({ 
    if (is.null(rv$user)) return(data.frame()) 
    rv$refresh 
    
    search <- input$searchBox 
    df <- load_recipes(rv$user$id, search) 
    
    if (nrow(df) == 0) return(data.frame()) 
    
    image_html <- sapply(df$image_path, create_image_html) 
    
    display_df <- data.frame( 
      ID = df$id, 
      Image = image_html, 
      Name = df$name, 
      Category = ifelse(is.na(df$category), "", df$category), 
      Difficulty = ifelse(is.na(df$difficulty), "", df$difficulty), 
      Time = paste0(df$prep_time + df$cook_time, " min"), 
      stringsAsFactors = FALSE 
    ) 
    
    datatable( 
      display_df, 
      selection = "single", 
      escape = FALSE, 
      options = list( 
        pageLength = 10,       
        dom = 'tip',             
        ordering = TRUE, 
        columnDefs = list( 
          list(visible = FALSE, targets = 0), 
          list(orderable = FALSE, targets = 1), 
          list(width = '80px', targets = 1) 
        ) 
      ), 
      rownames = FALSE 
    ) 
  }) 
  
  # Save button 
  observeEvent(input$saveBtn, { 
    if (is.null(rv$user)) return() 
    
    if (trimws(input$recipeName) == "") { 
      showNotification("Recipe name is required", type = "warning") 
      return() 
    } 
    
    imagePath <- rv$imagePath 
    if (!is.null(input$imageFile)) { 
      imagePath <- save_recipe_image(input$imageFile$datapath, input$recipeName) 
    } 
    
    rec <- list( 
      name = trimws(input$recipeName), 
      category = input$category, 
      difficulty = input$difficulty, 
      prep_time = ifelse(is.na(input$prepTime) || input$prepTime < 0, 0, input$prepTime), 
      cook_time = ifelse(is.na(input$cookTime) || input$cookTime < 0, 0, input$cookTime), 
      ingredients = trimws(input$ingredients), 
      instructions = trimws(input$instructions), 
      tags = trimws(input$tags), 
      image_path = imagePath 
    ) 
    
    if (is.null(rv$currentEditID)) { 
      if (insert_recipe(rec, rv$user$id)) { 
        showNotification(paste("Inserted:", rec$name), type = "message") 
        rv$refresh <- rv$refresh + 1 
        removeModal() 
        rv$imagePath <- "" 
        rv$currentEditID <- NULL 
      } 
    } else { 
      if (update_recipe_db(rv$currentEditID, rec)) { 
        showNotification(paste("Updated:", rec$name), type = "message") 
        rv$refresh <- rv$refresh + 1 
        removeModal() 
        rv$imagePath <- "" 
        rv$currentEditID <- NULL 
      } 
    } 
  }) 
  
  # Clear button 
  observeEvent(input$clearBtn, { 
    updateTextInput(session, "recipeName", value = "") 
    updateSelectInput(session, "category", selected = "Main") 
    updateSelectInput(session, "difficulty", selected = "Easy") 
    updateNumericInput(session, "prepTime", value = 0) 
    updateNumericInput(session, "cookTime", value = 0) 
    updateTextAreaInput(session, "ingredients", value = "") 
    updateTextAreaInput(session, "instructions", value = "") 
    updateTextInput(session, "tags", value = "") 
    rv$imagePath <- "" 
    showNotification("Form cleared", type = "message") 
  }) 
  
  # Helper function to show recipe modal 
  show_recipe_modal <- function(row) { 
    # Create image display 
    image_display <- if (!is.na(row$image_path) && row$image_path != "") { 
      full_path <- file.path("www/recipe_images", row$image_path) 
      if (file.exists(full_path)) { 
        tags$img(src = paste0("recipe_images/", row$image_path), class = "recipe-detail-image") 
      } else { 
        tags$div(style = "width:200px;height:200px;background:#141b2d;border-radius:12px;display:flex;align-items:center;justify-content:center;color:#94a3b8;font-size:48px;margin-bottom:20px;", "🍽️") 
      } 
    } else { 
      tags$div(style = "width:200px;height:200px;background:#141b2d;border-radius:12px;display:flex;align-items:center;justify-content:center;color:#94a3b8;font-size:48px;margin-bottom:20px;", "🍽️") 
    } 
    
    showModal(modalDialog( 
      title = paste("📖", row$name), 
      size = "l", 
      
      image_display, 
      
      div(class = "recipe-detail-section", 
          div(class = "recipe-detail-label", "Category & Difficulty"), 
          div(class = "recipe-detail-content", 
              paste(row$category, "•", row$difficulty) 
          ) 
      ), 
      
      div(class = "recipe-detail-section", 
          div(class = "recipe-detail-label", "Time"),
          div(class = "recipe-detail-content", 
              paste0("Prep: ", row$prep_time, " min • Cook: ", row$cook_time,  
                     " min • Total: ", (row$prep_time + row$cook_time), " min")
          ) 
      ), 
      
      div(class = "recipe-detail-section", 
          div(class = "recipe-detail-label", "Ingredients"), 
          div(class = "recipe-detail-content", row$ingredients) 
      ), 
      
      div(class = "recipe-detail-section", 
          div(class = "recipe-detail-label", "Instructions"), 
          div(class = "recipe-detail-content", row$instructions) 
      ), 
      
      if (!is.na(row$tags) && row$tags != "") { 
        div(class = "recipe-detail-section", 
            div(class = "recipe-detail-label", "Tags"), 
            div(class = "recipe-detail-content", row$tags) 
        ) 
      }, 
      
      easyClose = TRUE, 
      footer = modalButton("Close") 
    )) 
  } 
  
  # View button 
  observeEvent(input$viewBtn, { 
    if (is.null(rv$user)) return() 
    
    sel <- input$recipeTable_rows_selected 
    if (length(sel) == 0) { 
      showNotification("Select a recipe to view", type = "warning") 
      return() 
    } 
    
    search <- input$searchBox 
    df <- load_recipes(rv$user$id, search) 
    row <- df[sel, ] 
    show_recipe_modal(row) 
  }) 
  
  # Edit button 
  observeEvent(input$editBtn, { 
    if (is.null(rv$user)) return() 
    
    sel <- input$recipeTable_rows_selected 
    if (length(sel) == 0) { 
      showNotification("Select a recipe to edit", type = "warning") 
      return() 
    } 
    
    search <- input$searchBox 
    df <- load_recipes(rv$user$id, search) 
    row <- df[sel, ] 
    rv$currentEditID <- row$id 
    rv$imagePath <- ifelse(is.na(row$image_path), "", row$image_path) 
    
    removeModal() 
    
    showModal(modalDialog( 
      title = paste("✏ Edit Recipe:", row$name), 
      size = "l", 
      
      textInput("recipeName", "Recipe Name *", value = row$name), 
      selectInput("category", "Category *",  
                  choices = c("Main","Side","Appetizer","Snack","Dessert","Drink"), 
                  selected = ifelse(is.na(row$category), "Main", row$category)), 
      selectInput("difficulty", "Difficulty Level *", 
                  choices = c("Easy", "Medium", "Hard"), 
                  selected = ifelse(is.na(row$difficulty), "Easy", row$difficulty)), 
      
      fluidRow( 
        column(6, numericInput("prepTime", "Prep (min)", value = row$prep_time, min = 0)),
        column(6, numericInput("cookTime", "Cook (min)", value = row$cook_time, min = 0))
      ), 
      
      textAreaInput("ingredients", "Ingredients *", height = "150px", value = row$ingredients), 
      textAreaInput("instructions", "Instructions *", height = "150px", value = row$instructions), 
      textInput("tags", "Tags", value = ifelse(is.na(row$tags), "", row$tags)), 
      
      fileInput("imageFile", "Recipe Image", accept = c('image/png', 'image/jpeg', 'image/jpg')), 
      textOutput("imagePreview"), 
      br(), 
      textOutput("statusMessage"), 
      
      footer = tagList( 
        modalButton("Cancel"), 
        actionButton("saveBtn", "💾 Save Changes", class = "btn-primary"), 
        actionButton("clearBtn", "🗑 Clear Form", class = "btn-secondary") 
      ), 
      easyClose = FALSE 
    )) 
  }) 
  
  # Delete button 
  observeEvent(input$deleteBtn, { 
    if (is.null(rv$user)) return() 
    
    sel <- input$recipeTable_rows_selected 
    if (length(sel) == 0) { 
      showNotification("Select a recipe to delete", type = "warning") 
      return() 
    } 
    
    search <- input$searchBox 
    df <- load_recipes(rv$user$id, search) 
    row <- df[sel, ] 
    
    showModal(modalDialog( 
      title = "Confirm Delete", 
      paste("Are you sure you want to delete:", row$name, "?"), 
      footer = tagList( 
        modalButton("Cancel"), 
        actionButton("confirmDelete", "Delete", class = "btn-danger") 
      ) 
    )) 
  }) 
  
  # Confirm delete 
  observeEvent(input$confirmDelete, { 
    if (is.null(rv$user)) return() 
    
    sel <- input$recipeTable_rows_selected 
    if (length(sel) > 0) { 
      search <- input$searchBox 
      df <- load_recipes(rv$user$id, search) 
      row <- df[sel, ] 
      if (delete_recipe_db(row$id)) { 
        showNotification(paste("Deleted:", row$name), type = "message") 
        rv$refresh <- rv$refresh + 1 
        removeModal() 
      } 
    } 
  }) 
  
  # Auto-reset dropdowns 
  observeEvent(input$singleFilter, { 
    if (!is.null(input$singleFilter) && input$singleFilter != "") { 
      updateSelectInput(session, "pairingFilter", selected = "") 
      updateSelectInput(session, "setFilter", selected = "") 
    } 
  }) 
  
  observeEvent(input$pairingFilter, { 
    if (!is.null(input$pairingFilter) && input$pairingFilter != "") { 
      updateSelectInput(session, "singleFilter", selected = "") 
      updateSelectInput(session, "setFilter", selected = "") 
    } 
  }) 
  
  observeEvent(input$setFilter, { 
    if (!is.null(input$setFilter) && input$setFilter != "") { 
      updateSelectInput(session, "singleFilter", selected = "") 
      updateSelectInput(session, "pairingFilter", selected = "") 
    } 
  }) 
  
  # Find meal combinations 
  observeEvent(input$findMealsBtn, { 
    if (is.null(rv$user)) return() 
    
    target <- input$targetTime 
    tolerance <- input$timeTolerance 
    
    combo_filter <- "" 
    
    if (!is.null(input$singleFilter) && input$singleFilter != "") { 
      combo_filter <- input$singleFilter 
    } else if (!is.null(input$pairingFilter) && input$pairingFilter != "") { 
      combo_filter <- input$pairingFilter 
    } else if (!is.null(input$setFilter) && input$setFilter != "") { 
      combo_filter <- input$setFilter 
    } 
    
    if (combo_filter == "") { 
      showNotification("Please select a meal combination type", type = "warning") 
      return() 
    } 
    
    if (is.na(target) || target <= 0) { 
      showNotification("Please enter a valid time", type = "warning") 
      return() 
    } 
    
    showNotification("Searching for meal combinations...", type = "message", duration = 2) 
    
    combinations <- find_meal_combinations(rv$user$id, target, tolerance, combo_filter, input$difficultyFilter) 
    rv$mealCombinations <- combinations 
    
    if (length(combinations) == 0) { 
      showNotification("No combinations found for this time range", type = "warning") 
    } else { 
      showNotification(paste("Found", length(combinations), "meal combinations!"), type = "message") 
    } 
  }) 
  
  # Click recipe chip 
  observeEvent(input$viewRecipeFromChip, { 
    if (is.null(rv$user)) return() 
    
    recipe_id <- input$viewRecipeFromChip 
    recipe_row <- rv$recipes[rv$recipes$id == recipe_id, ] 
    if (nrow(recipe_row) > 0) { 
      show_recipe_modal(recipe_row) 
    } 
  }) 
  
  # Render meal combinations 
  output$mealCombinations <- renderUI({ 
    combinations <- rv$mealCombinations 
    
    if (length(combinations) == 0) { 
      return(div(class = "no-results", "No combinations found. Try adjusting your time, tolerance, or filter settings.")) 
    } 
    
    combination_items <- lapply(1:min(length(combinations), 20), function(i) { 
      combo <- combinations[[i]] 
      recipes <- combo$recipes 
      total_time <- combo$total_time 
      combo_size <- combo$combination_size 
      combo_type <- if (!is.null(combo$combo_type)) combo$combo_type else paste(combo_size, "Dish Combo") 
      
      # Create clickable recipe chips with larger images 
      recipe_chips <- lapply(recipes, function(rec) { 
        # Create image element (larger) 
        image_elem <- if (!is.na(rec$image_path) && rec$image_path != "") { 
          full_path <- file.path("www/recipe_images", rec$image_path) 
          if (file.exists(full_path)) { 
            tags$img( 
              src = paste0("recipe_images/", rec$image_path), 
              style = "width:200px; height:200px; object-fit:cover; border-radius:12px; border:2px solid #141b2d;" 
            ) 
          } else { 
            tags$div( 
              style = "width:200px; height:200px; background:#1e2739; border-radius:12px; display:flex; align-items:center; justify-content:center; font-size:40px;", 
              "🍽️" 
            ) 
          } 
        } else { 
          tags$div( 
            style = "width:200px; height:200px; background:#1e2739; border-radius:12px; display:flex; align-items:center; justify-content:center; font-size:40px;", 
            "🍽️" 
          ) 
        } 
        
        tags$div( 
          class = "recipe-chip", 
          onclick = paste0("Shiny.setInputValue('viewRecipeFromChip', ", rec$id, ", {priority: 'event'});"), 
          style = "flex-direction: column; align-items: center; padding:10px; gap:6px;", 
          image_elem, 
          tags$div( 
            style = "display: flex; flex-direction: column; align-items:center; gap:2px;", 
            tags$div( 
              style = "display: flex; align-items: center; gap:6px;", 
              tags$span(rec$name), 
              tags$span(class = "category-badge", rec$category) 
            ), 
            tags$span( 
              style = "color: #94a3b8; font-size: 12px;",  
              paste0("⏱ ", rec$total_time, " min • 🍽 ", rec$difficulty) 
            ) 
          ) 
        ) 
      }) 
      
      # Combination type icon 
      combo_icon <- switch(as.character(combo_size), 
                           "1" = "🍽", 
                           "2" = "🍽🍽", 
                           "3" = "🍽🍽🍽", 
                           "🍽") 
      
      div(class = "combination-card", 
          div(class = "combination-header", 
              combo_icon, " ", combo_type, 
              tags$span(class = "time-badge", style = "float: right;", paste(total_time, "min")) 
          ), 
          div(style = "display: flex; flex-wrap: wrap; gap:10px;", recipe_chips) 
      ) 
    }) 
    
    div( 
      tags$hr(style = "border-color: #2d3748; margin: 20px 0;"), 
      div(style = "color: #dc2626; font-weight: 600; margin-bottom: 15px;", 
          paste("Showing", min(length(combinations), 20), "of", length(combinations), "combinations")), 
      combination_items 
    ) 
  }) 
} 

# Run the app 
shinyApp(ui = ui, server = server)