-- MySQL dump 10.13  Distrib 5.7.28, for macos10.14 (x86_64)
--
-- Host: localhost    Database: croe
-- ------------------------------------------------------
-- Server version	5.7.28

/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;
/*!40103 SET @OLD_TIME_ZONE=@@TIME_ZONE */;
/*!40103 SET TIME_ZONE='+00:00' */;
/*!40014 SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0 */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;
/*!40111 SET @OLD_SQL_NOTES=@@SQL_NOTES, SQL_NOTES=0 */;

--
-- Table structure for table `review`
--

DROP TABLE IF EXISTS `review`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `review` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT,
  `user` bigint(20) NOT NULL,
  `rating` double NOT NULL,
  `message` text CHARACTER SET utf8,
  PRIMARY KEY (`id`),
  KEY `review_user_fkey` (`user`),
  CONSTRAINT `review_user_fkey` FOREIGN KEY (`user`) REFERENCES `user` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `review`
--

LOCK TABLES `review` WRITE;
/*!40000 ALTER TABLE `review` DISABLE KEYS */;
/*!40000 ALTER TABLE `review` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `school`
--

DROP TABLE IF EXISTS `school`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `school` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT,
  `name` text CHARACTER SET utf8 NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=3 DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `school`
--

LOCK TABLES `school` WRITE;
/*!40000 ALTER TABLE `school` DISABLE KEYS */;
INSERT INTO `school` VALUES (1,'中山大学'),(2,'测试学校(google)');
/*!40000 ALTER TABLE `school` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `school_campus`
--

DROP TABLE IF EXISTS `school_campus`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `school_campus` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT,
  `school_id` bigint(20) NOT NULL,
  `name` text CHARACTER SET utf8 NOT NULL,
  PRIMARY KEY (`id`),
  KEY `school_campus_school_id_fkey` (`school_id`),
  CONSTRAINT `school_campus_school_id_fkey` FOREIGN KEY (`school_id`) REFERENCES `school` (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=2 DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `school_campus`
--

LOCK TABLES `school_campus` WRITE;
/*!40000 ALTER TABLE `school_campus` DISABLE KEYS */;
INSERT INTO `school_campus` VALUES (1,1,'东校区');
/*!40000 ALTER TABLE `school_campus` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `school_domain`
--

DROP TABLE IF EXISTS `school_domain`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `school_domain` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT,
  `school_id` bigint(20) NOT NULL,
  `domain` text CHARACTER SET utf8 NOT NULL,
  PRIMARY KEY (`id`),
  KEY `school_domain_school_id_fkey` (`school_id`),
  CONSTRAINT `school_domain_school_id_fkey` FOREIGN KEY (`school_id`) REFERENCES `school` (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=3 DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `school_domain`
--

LOCK TABLES `school_domain` WRITE;
/*!40000 ALTER TABLE `school_domain` DISABLE KEYS */;
INSERT INTO `school_domain` VALUES (1,1,'mail2.sysu.edu.cn'),(2,2,'gmail.com');
/*!40000 ALTER TABLE `school_domain` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `task`
--

DROP TABLE IF EXISTS `task`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `task` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT,
  `current_status` text CHARACTER SET utf8 NOT NULL,
  `creator` bigint(20) NOT NULL,
  `taker` bigint(20) DEFAULT NULL,
  `reward` bigint(20) NOT NULL,
  `title` text CHARACTER SET utf8 NOT NULL,
  `location` bigint(20) NOT NULL,
  `duration` text CHARACTER SET utf8 NOT NULL,
  `abstract` text CHARACTER SET utf8 NOT NULL,
  `description_key` text CHARACTER SET utf8 NOT NULL,
  PRIMARY KEY (`id`),
  KEY `task_creator_fkey` (`creator`),
  KEY `task_taker_fkey` (`taker`),
  KEY `task_location_fkey` (`location`),
  CONSTRAINT `task_creator_fkey` FOREIGN KEY (`creator`) REFERENCES `user` (`id`),
  CONSTRAINT `task_location_fkey` FOREIGN KEY (`location`) REFERENCES `school_campus` (`id`),
  CONSTRAINT `task_taker_fkey` FOREIGN KEY (`taker`) REFERENCES `user` (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=4 DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `task`
--

LOCK TABLES `task` WRITE;
/*!40000 ALTER TABLE `task` DISABLE KEYS */;
/*!40000 ALTER TABLE `task` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `user`
--

DROP TABLE IF EXISTS `user`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `user` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT,
  `email` text CHARACTER SET utf8 NOT NULL,
  `name` text CHARACTER SET utf8 NOT NULL,
  `hashed_password` blob NOT NULL,
  `role` text CHARACTER SET utf8 NOT NULL,
  `balance` bigint(20) NOT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `unique_user_email` (`email`(200))
) ENGINE=InnoDB AUTO_INCREMENT=2 DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `user`
--

LOCK TABLES `user` WRITE;
/*!40000 ALTER TABLE `user` DISABLE KEYS */;
INSERT INTO `user` VALUES (1,'fengzlin@mail2.sysu.edu.cn','fzl',0x2432622431322434757362424672336E4F516B6C7441716C58374F4B75344557504D564F317A61334167354570526B584D39516361784F4D2F305061,'RoleUser',0);
/*!40000 ALTER TABLE `user` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `user_registry`
--

DROP TABLE IF EXISTS `user_registry`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `user_registry` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT,
  `email` text CHARACTER SET utf8 NOT NULL,
  `created_at` datetime NOT NULL,
  `code` text CHARACTER SET utf8 NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=39 DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `user_registry`
--

LOCK TABLES `user_registry` WRITE;
/*!40000 ALTER TABLE `user_registry` DISABLE KEYS */;
INSERT INTO `user_registry` VALUES (36,'fengzlin@mail2.sysu.edu.cn','2020-02-04 01:49:39','648713');
/*!40000 ALTER TABLE `user_registry` ENABLE KEYS */;
UNLOCK TABLES;
/*!40103 SET TIME_ZONE=@OLD_TIME_ZONE */;

/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;

-- Dump completed on 2020-02-12  8:10:50
