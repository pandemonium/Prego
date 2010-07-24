package se.jwzrd.prego.core.database

import java.util.Date
import org.squeryl.Schema

/**
 * @author Patrik Andersson <pandersson@gmail.com>
 */
import org.squeryl.PrimitiveTypeMode._
import org.squeryl.adapters.MySQLAdapter

trait DatabaseInfrastructure {
  def bootstrap {
    import java.sql.DriverManager
    import org.squeryl.{SessionFactory, Session}

    Class forName "com.mysql.jdbc.Driver" newInstance

    SessionFactory.concreteFactory = Some (() =>
      Session.create (DriverManager getConnection ("jdbc:mysql://quux.local/prego?user=root"),
                      new MySQLAdapter))
  }

  bootstrap
}





