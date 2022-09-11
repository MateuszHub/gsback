use gamestore;
SET NAMES 'utf8mb4' COLLATE 'utf8mb4_bin';

CREATE TABLE IF NOT EXISTS `logins` (
    `id` MEDIUMINT NOT NULL AUTO_INCREMENT,
    `email`  VARCHAR(255) BINARY UNIQUE,
    `password`  varchar(255) BINARY,
    PRIMARY KEY (id)
);

CREATE TABLE IF NOT EXISTS `users` (
    `id` MEDIUMINT NOT NULL AUTO_INCREMENT,
    `email`  VARCHAR(255) BINARY UNIQUE,
    `name`  varchar(255) BINARY,
    `img` varchar(255),
    PRIMARY KEY (id)
);

CREATE TABLE IF NOT EXISTS `items` (
    `id` MEDIUMINT NOT NULL AUTO_INCREMENT,
    `name` varchar(255),
    `desc` TEXT,
    `price` MEDIUMINT,
    `active` BOOLEAN DEFAULT TRUE,
    `img` varchar(255) DEFAULT NULL,
    PRIMARY KEY (`id`)
);

CREATE TABLE IF NOT EXISTS `adresses` (
    `id` MEDIUMINT NOT NULL AUTO_INCREMENT,
    `user_id` MEDIUMINT ,
    `line1` TEXT,
    `line2` TEXT,
    `city` TEXT,
    `country` TEXT,
    `zip` TEXT,
    PRIMARY KEY (`id`),
    FOREIGN KEY (user_id) REFERENCES users(id)
);

CREATE TABLE IF NOT EXISTS `orders` (
    `id` MEDIUMINT NOT NULL AUTO_INCREMENT,
    `user_id` MEDIUMINT ,
    `when` Date,
    `total` MEDIUMINT,
    `paid` MEDIUMINT,
    `link` TEXT,
    `payId` TEXT,
    PRIMARY KEY (`id`),
    FOREIGN KEY (user_id) REFERENCES users(id)
);


CREATE TABLE IF NOT EXISTS `items_orders` (
    `id` MEDIUMINT NOT NULL AUTO_INCREMENT,
    `item_id` MEDIUMINT ,
    `order_id` MEDIUMINT,
    PRIMARY KEY (`id`),
    FOREIGN KEY (item_id) REFERENCES items(id),
    FOREIGN KEY (order_id) REFERENCES orders(id)
);