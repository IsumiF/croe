-- MySQL dump 10.13  Distrib 5.7.28, for macos10.14 (x86_64)
--
-- Host: 127.0.0.1    Database: croe
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

USE croe_test;

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
  PRIMARY KEY (`id`),
  UNIQUE KEY `unique_user_email` (`email`(200))
) ENGINE=InnoDB AUTO_INCREMENT=2 DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `user`
--

LOCK TABLES `user` WRITE;
/*!40000 ALTER TABLE `user` DISABLE KEYS */;
INSERT INTO `user` VALUES (1,'fengzlin@mail2.sysu.edu.cn','fzl',0x243262243132246C66337348585353615964446B7561327A592F7347657A482F3734524D5352786D4A62586B4B596436744963796D743347376E7547,'User');
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
) ENGINE=InnoDB AUTO_INCREMENT=17 DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `user_registry`
--

LOCK TABLES `user_registry` WRITE;
/*!40000 ALTER TABLE `user_registry` DISABLE KEYS */;
INSERT INTO `user_registry` VALUES (15,'steven172307@gmail.com','2020-01-14 13:40:15','650045'),(16,'steven172307@gmail.com','2020-01-26 03:31:37','248952');
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

-- Dump completed on 2020-01-26 11:33:29
