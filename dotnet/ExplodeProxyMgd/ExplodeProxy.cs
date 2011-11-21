using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Autodesk.AutoCAD.Runtime;
using Autodesk.AutoCAD.DatabaseServices;
using Autodesk.AutoCAD.ApplicationServices;

namespace ExplodeProxyMgd
{
    public class ExplodeProxy
    {
        [LispFunction("proxy-explode-entity")]
        public static TypedValue ProxyExplodeEntity(ResultBuffer rbArgs)
        {
            Document doc = Application.DocumentManager.MdiActiveDocument;
            Database db = doc.Database;
            TypedValue res = new TypedValue((int)LispDataType.Text,"");
            if (rbArgs != null)
            {
                foreach (TypedValue rb in rbArgs)
                {
                    if (rb.TypeCode == (int)LispDataType.ObjectId)
                    {
                        using (Transaction tr = doc.TransactionManager.StartTransaction())
                        {
                            ObjectId id = (ObjectId)rb.Value;
                            DBObjectCollection objs = new DBObjectCollection();
                            BlockTable bt = (BlockTable)tr.GetObject(db.BlockTableId, OpenMode.ForRead);

                            Entity entx = (Entity)tr.GetObject(id, OpenMode.ForWrite);
                            entx.Explode(objs);

                            string blkName = "ncs-acad-" + entx.Handle.ToString();

                            if (bt.Has(blkName) == false)
                            {
                                BlockTableRecord btr = new BlockTableRecord();
                                btr.Name = blkName;

                                bt.UpgradeOpen();
                                ObjectId btrId = bt.Add(btr);
                                tr.AddNewlyCreatedDBObject(btr, true);

                                foreach (DBObject obj in objs)
                                {
                                    Entity ent = (Entity)obj;
                                    btr.AppendEntity(ent);
                                    tr.AddNewlyCreatedDBObject(ent, true);
                                }
                            }
                            res = new TypedValue((int)LispDataType.Text, blkName);

                            tr.Commit();
                        }
                    }
                }
            }
            return res;
        }
    }
}
